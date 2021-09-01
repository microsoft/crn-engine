// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import { IDataSet, IDataSetStorage } from './crnDataSets'

// (FP) This file contains several implementations of the IDataSetStorage interface, i.e. several different ways to store a set of
// observation data points.

/**
 * Storing dataset files via IndexedDB. (FP) IndexedDB is a native database-like API. It stores JSON objects locally and offline. Note that this is the
 * one that's actually used in the tools (at the time of writing).
 */
export class IndexedDBDataSetStorage implements IDataSetStorage {
    private indexedDB = window.indexedDB;
    private database: IDBDatabase;
    private connectdfd = jQuery.Deferred();
    private storeName = "datasets";
    constructor(private dbName: string) {
        this.ConnectDB(dbName);
    }
    private ConnectDB(dbName: string) {
        var request = indexedDB.open(dbName, 3);
        request.onerror = (err) => this.connectdfd.reject(err);
        request.onsuccess = () => {
            this.database = request.result;
            this.connectdfd.resolve();
        }
        request.onupgradeneeded = (e: Event) => {
            let names: DOMStringList = (<IDBRequest>e.currentTarget).result.objectStoreNames;
            if (names.contains(this.storeName)) {
                (<IDBRequest>e.currentTarget).result.deleteObjectStore(this.storeName);
            }
            (<IDBRequest>e.currentTarget).result.createObjectStore(this.storeName);
            this.ConnectDB(dbName);
        }
    }

    Name(): string {
        return this.dbName;
    }

    Add(name: string, data: IDataSet): JQueryPromise<void> {
        var dfd = jQuery.Deferred<void>();
        this.connectdfd.done(() => {
            var store = this.database.transaction([this.storeName], "readwrite").objectStore(this.storeName);
            var request = store.put(JSON.stringify(data), name);
            request.onsuccess = () => {
                dfd.resolve();
            };
            request.onerror = (err) => dfd.reject(err);
        });
        this.connectdfd.fail(() => dfd.reject("Connection to " + this.dbName + " failed."));
        return dfd.promise();
    }
    Get(name: string) {
        var dfd = jQuery.Deferred<IDataSet>();
        this.connectdfd.done(() => {
            var store = this.database.transaction([this.storeName], "readonly").objectStore(this.storeName);
            var request = store.get(name);
            request.onsuccess = () => {
                if (request.result == null)
                    dfd.reject("DataSetStorage.Get could not find " + name);
                else
                    try {
                        var res = JSON.parse(request.result);
                        dfd.resolve(res);
                    }
                    catch (e) {
                        dfd.reject("DataSetStorage.Get threw " + JSON.stringify(e));
                    }
            };
            request.onerror = (err) => dfd.reject(err);
        });
        this.connectdfd.fail(() => dfd.reject("Connection to " + this.dbName + " failed."));
        return dfd.promise();
    }

    GetNames() {
        var dfd = jQuery.Deferred<string[]>();
        this.connectdfd.done(() => {
            var rows: any[] = [];
            var store = this.database.transaction([this.storeName], "readonly").objectStore(this.storeName);
            var cursor = store.openCursor();
            cursor.onsuccess = function (e: Event) {
                var cursor = (<IDBRequest>e.target).result;
                if (cursor) {
                    rows.push(cursor.key);
                    cursor.continue();
                }
                else {
                    dfd.resolve(rows);
                }
            };
            cursor.onerror = (err) => dfd.reject(err);
        });
        this.connectdfd.fail(() => dfd.reject("Connection to " + this.dbName + " failed."));
        return dfd.promise();
    }

    Remove(name: string): JQueryPromise<void> {
        var dfd = jQuery.Deferred<void>();
        if (name == null)
            dfd.reject();
        else {
            this.connectdfd.done(() => {
                var store = this.database.transaction([this.storeName], "readwrite").objectStore(this.storeName);
                store.delete(name).onsuccess = () => {
                    dfd.resolve();
                };
            });
            this.connectdfd.fail(() => dfd.reject("Connection to " + this.dbName + " failed."));
        }
        return dfd.promise();
    }
}

/**
 * Storing dataset files in array of IDataSet. (FP) This implementation stores the data in memory (so it's wiped when the session ends).
 */
export class MemoryDataSetStorage implements IDataSetStorage {
    private data: { name: string; data: IDataSet }[] = [];
    Add(name: string, data: IDataSet) {
        var dfd = jQuery.Deferred<void>();
        this.data.push({ name: name, data: data });
        dfd.resolve();
        return dfd.promise();
    }
    Get(name: string) {
        var dfd = jQuery.Deferred<IDataSet>();
        let ret = this.data.filter(val => val.name === name);
        dfd.resolve(ret.length ? ret[0].data : null);
        return dfd.promise();

    }
    GetNames() {
        var dfd = jQuery.Deferred<string[]>();
        dfd.resolve(this.data.map(val => val.name));
        return dfd.promise();
    }
    Remove(name: string) {
        var dfd = jQuery.Deferred<void>();
        this.data = this.data.filter(val => val.name !== name);
        dfd.resolve();
        return dfd.promise();
    }
}

/**
 * Storing dataset files in LocalStorage. (FP) LocalStorage is a native API to store objects locally and offline.
 */
export class LocalStorageDataSetStorage implements IDataSetStorage {
    private static storage = window.localStorage;
    private static instance: LocalStorageDataSetStorage;

    Add(name: string, data: IDataSet) {
        var dfd = jQuery.Deferred<void>();
        LocalStorageDataSetStorage.storage.setItem(name, JSON.stringify(data));
        dfd.resolve();
        return dfd.promise();
    }
    Get(name: string) {
        var dfd = jQuery.Deferred<IDataSet>();
        dfd.resolve(JSON.parse(LocalStorageDataSetStorage.storage.getItem(name)));
        return dfd.promise();
    }
    GetNames() {
        var dfd = jQuery.Deferred<string[]>();
        let names: string[] = [];
        for (let i = 0; i < LocalStorageDataSetStorage.storage.length; i++)
            names.push(LocalStorageDataSetStorage.storage.key(i));
        dfd.resolve(names);
        return dfd.promise();
    }
    Remove(name: string) {
        var dfd = jQuery.Deferred<void>();
        LocalStorageDataSetStorage.storage.removeItem(name);
        dfd.resolve();
        return dfd.promise();
    }

    static GetInstance(): LocalStorageDataSetStorage {
        if (!this.instance) this.instance = new LocalStorageDataSetStorage();
        return this.instance;
    }
}
