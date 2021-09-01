// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ParseOperation from '../Operations/GenericCodeParsing';
import * as CRNParseOperation from '../Operations/ParseCodeFillCRN';
import * as serialization from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as jQuery from 'jquery';

export interface IModelStreams<TCustomSettings> {
    model: Rx.Observable<serialization.IG>,
    customSettings?: Rx.Observable<TCustomSettings>,
}

//Uses ModellingEngine.ts layer to aquire parsed CRN code
export abstract class Parser<TOptions, TCustomSettings> implements ParseOperation.IParser<CRNParseOperation.IParsed<TCustomSettings>, TOptions> {
    abstract getModelStreams(code: string, options: TOptions, server?: boolean): IModelStreams<TCustomSettings>;

    abstract failureHandler(failure: any): void;
    //ParseFeature.ICRNParser implementation
    public TryParse(code: string, options: TOptions, server?: boolean): JQueryDeferred<ParseOperation.IParsingResults<CRNParseOperation.IParsed<TCustomSettings>>> {
        var dfd = jQuery.Deferred<ParseOperation.IParsingResults<CRNParseOperation.IParsed<TCustomSettings>>>();
        var crnStreams = this.getModelStreams(code, options, server);

        var loc_model: serialization.IG = null;
        var loc_cust: TCustomSettings = null;
        if (crnStreams.customSettings != null)
            crnStreams.customSettings.subscribe(cust => {
                if (loc_model == null)
                    loc_cust = cust;
                else
                    dfd.resolve({
                        GetStatus: () => ParseOperation.ParsingStatus.Success,
                        GetData: () => {
                            return { Model: loc_model, CustomSettings: cust };
                        }
                    });
            });

        crnStreams.model.subscribe(model => {
            if (crnStreams.customSettings != null && loc_cust == null)
                loc_model = model;
            else
                dfd.resolve({
                    GetStatus: () => ParseOperation.ParsingStatus.Success,
                    GetData: () => {
                        return { Model: model, CustomSettings: loc_cust };
                    }
                });
        }, (error: serialization.Error) => {
            if ((<serialization.ParsingError>error).message != null) {
                var parsingError = <serialization.ParsingError>error;
                console.log(parsingError.message);
                var errors: ParseOperation.ErrorList = [];
                if ((<serialization.ParsingError>error).positions && (<serialization.ParsingError>error).positions.length > 0)
                    parsingError.positions.forEach(p => errors.push({ location: { rowStart: p.row, colStart: p.column, rowEnd: p.row, colEnd: p.column }, text: "Line " + p.row + ", column " + p.column + ": " + p.text }));
                else
                    errors.push({ text: parsingError.message });
                dfd.resolve(
                    {
                        GetStatus: () => ParseOperation.ParsingStatus.InvalidCode,
                        GetData: () => errors
                    }
                );
            }
            else {
                dfd.resolve(
                    {
                        GetStatus: () => ParseOperation.ParsingStatus.InternalError,
                        GetData: () => [{ text: error.message }]
                    }
                );
            }
        }, () => { });

        dfd.fail((failure: any) => this.failureHandler);
        return dfd;
    }
}
