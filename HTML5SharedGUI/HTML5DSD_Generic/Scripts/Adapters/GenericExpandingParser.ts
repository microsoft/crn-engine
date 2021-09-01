// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ParseOperation from '../../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/GenericCodeParsing';
import * as CRNParseOperation from '../../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/ParseCodeFillCRN';
import * as DSDParseOperation from '../Operations/ParseSgCode';
import * as serialization from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as jQuery from 'jquery';

export interface IResult<TCalcSettings> {
    model: serialization.IG,
    settings: TCalcSettings,
    code: string
}
export interface IResultStreams<TCalcSettings> {
    result: Rx.Observable<IResult<TCalcSettings>>,
}

//Uses ModellingEngine.ts layer to aquire parsed CRN code
export abstract class ExpandingParser<TCalcSettings, TOptions> implements ParseOperation.IParser<DSDParseOperation.IParsed<TCalcSettings>, TOptions> {

    abstract getCRNStreams(code: string, options: TOptions, model: serialization.IG, calcSettings: TCalcSettings, server?: boolean): IResultStreams<TCalcSettings>;
    abstract failureHandler(failure: any): void;

    private model: serialization.IG;
    private settings: TCalcSettings;
    public SetCurrentState(model: serialization.IG, settings: TCalcSettings): void {
        this.model = model;
        this.settings = settings;
    }

    //ParseOperation.IParser implementation
    public TryParse(code: string, options: TOptions, server?: boolean): JQueryDeferred<ParseOperation.IParsingResults<DSDParseOperation.IParsed<TCalcSettings>>> {
        var dfd = jQuery.Deferred<ParseOperation.IParsingResults<DSDParseOperation.IParsed<TCalcSettings>>>();
        var crnStreams = this.getCRNStreams(code, options, this.model, this.settings, server);

        crnStreams.result.subscribe(result => {
            dfd.resolve({
                GetStatus: () => ParseOperation.ParsingStatus.Success,
                GetData: () => {
                    return { model: result.model, customSettings: result.settings, code: result.code }
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
