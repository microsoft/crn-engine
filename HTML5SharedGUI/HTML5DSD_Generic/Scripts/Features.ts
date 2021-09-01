// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//bind to specific DOM element and occupy it
export interface IUIBindable {
    Bind(elem: HTMLElement): void;
}

//bind to the DOM, but automatically finds out a place to bind (e.g. by predefined element id)
export interface IUIAutoBindable {
    AutoBind(): void;
}
