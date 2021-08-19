//import crn = require("../../CRNEngine/Interfaces");
import { WebSharperGeneratedInterfaces as WGI } from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/WebSharperGeneratedInterfaces';
import { IDataSetStorage, IDataSet } from '../../../../HTML5SharedGUI/CRNComponent/Scripts/crnDataSets';
import * as InferenceOp from '../Operations/Inference';
import * as $ from 'jquery';

import CRNEngine = WGI.Microsoft.Research.CRNEngine;

/**
 * Convert UI dataset (set of rows) into serializable dataset (sets of columns)
 * @param data containerof the data
 * @param name A name to assign to the results
 * @param plotsCount the number of columns in each resulting table.t. Controls how data columns are mapped to list of table.t with list of columns inside
 */
export function Convert(data: IDataSet, name: string, plotsCount: number): CRNEngine.Dataset {
    var transposedData: number[][] = [];
    var rawdata = data.Data;
    var rowCount = rawdata.length;
    var colCount = rawdata[0].length;

    for (var i = 0; i < colCount; i++)
        transposedData[i] = [];
    for (var i = 0; i < rowCount; i++)
        for (var j = 0; j < colCount; j++) {
            // Enforce float type.
            var v = rawdata[i][j];
            if (typeof v == "string")
                v = parseFloat(v);
            transposedData[j][i] = v;
        }

    var time = transposedData[0];
    var result: CRNEngine.Dataset = {
        file: name,
        data: []
    }

    var tablesCount = (colCount - 1) / plotsCount;
    var rem = (colCount - 1) % plotsCount
    if (rem != 0.0)
        throw "Dataset \"" + name + "\" columns count is not a multiple of number of plots (" + plotsCount + ")";
    for (var t = 0; t < tablesCount; t++)
        result.data.push({
            times: time,
            columns: []
        });

    for (var i = 1; i < data.ColumnNames.length; i++) {
        var name = data.ColumnNames[i];
        var res = transposedData[i];
        var column: CRNEngine.Column<number> = {
            name: name,
            values: res
        }
        var tableIdx = Math.floor((i - 1) / plotsCount);
        result.data[tableIdx].columns.push(column);
    }
    return result;
}

/**
 * A source of observations that grabs selected files contents from IDataSetStorage and returns the data
 */
export class ObservationsSource implements InferenceOp.IObservationsSource {
    /** @param storage A storage of datasets */
    constructor(private storage: IDataSetStorage) { }

    //implementantion of InferenceOp.IObseravationsSource
    public GetData(sets: InferenceOp.IObservationsDef[]): JQueryPromise<CRNEngine.Dataset[]> {
        var dfd = $.Deferred<CRNEngine.Dataset[]>();

        var selected: IDataSet[] = [];
        var result: CRNEngine.Dataset[] = [];
        var promises: JQueryPromise<IDataSet>[] = [];
        for (let val of sets.map(s => s.name)) {
            promises.push(this.storage.Get(val).done((res: IDataSet) => {
                selected.push(res);
            }).fail(() => dfd.reject(val + " not found in storage")));
        }

        //Converting DataSets.IDataSets to crn.Table[]
        $.when.apply(this, promises).done(() => {
            //transposing
            for (let k = 0; k < selected.length; k++) {
                try {
                    result.push(Convert(selected[k], sets[k].name, sets[k].plotsCount));
                }
                catch (exc) {
                    dfd.reject(exc);
                }
            }
            dfd.resolve(result);
        });

        return dfd.promise();
    }
}