// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as $ from 'jquery';
import * as Operations from "./LongOperations";
import * as Rx from "rx";

//Dependencies to be injected

export interface IHighlight {
    rowStart: number;
    colStart: number;
    rowEnd: number;
    colEnd: number;
}

export interface IError {
    location?: IHighlight;
    text: string;
}

export enum ParsingStatus { Success, InvalidCode, InternalError };

export type ErrorList = Array<IError>;

export interface IParsingResults<TParsed> {
    GetStatus(): ParsingStatus;
    GetData(): TParsed | ErrorList;
}

export interface IParser<TParsed, TOptions> {
    TryParse(code: string, options: TOptions): JQueryDeferred<IParsingResults<TParsed>>;
}

//provides the user input upon request
export interface ICodeSource<TOptions> {
    GetCode(): string;
    GetParserOptions(): TOptions;
    EditorLoadEvents: Rx.Observable<string>;
}

//Displays a set of error strings
export interface IErrorDisplay {
    ShowErrors(errors: Array<IError>): void;
    ClearErrors(): void;
}

//End of dependencies to be injected


/**
 * Generic parsing as Operations.IOperation: take the code from the code source, parse it with parser, handler errors or interpret successful results.
 * handles invalid syntax errors
 * handles parser failures
 * hanldle successful paring with InterpretSuccessfulResults abstract function invokation
 */
export abstract class Operation<TParsed, TOptions> implements Operations.IOperation {
    private ongoingParserPromise: JQueryDeferred<IParsingResults<TParsed>> = undefined; //retured by parser
    private initiatedDeferred: JQueryDeferred<any> = undefined; //operation promise
    private exitMessage = "";

    constructor(private codeSource: ICodeSource<TOptions>, private parser: IParser<TParsed, TOptions>, private errorDisplay: IErrorDisplay) {
    }

    //Operations.IOperation implementation
    public abstract GetName(): string;

    public GetExitMessage() {
        return this.exitMessage;
    }

    //actions to perform upon successful parsing
    protected abstract InterpretSuccessfulResults(data: TParsed): void;

    public Initiate(): JQueryPromise<any> {
        var code = this.codeSource.GetCode();
        var options = this.codeSource.GetParserOptions();
        this.ongoingParserPromise = this.parser.TryParse(code, options);
        this.initiatedDeferred = jQuery.Deferred<any>();
        this.ongoingParserPromise
            .fail((error: any) => {
                console.log("Parser failed with: " + JSON.stringify(error));
                this.exitMessage = JSON.stringify(error);
                this.initiatedDeferred.reject();
            })
            .done((results: IParsingResults<TParsed>) => {
                switch (results.GetStatus()) {
                    case ParsingStatus.Success:
                        var data = <TParsed>results.GetData();
                        this.InterpretSuccessfulResults(data);  // <--- this action vary in deffered classes                                                
                        this.errorDisplay.ClearErrors();
                        this.exitMessage = "completed";
                        this.initiatedDeferred.resolve(results);
                        break;
                    case ParsingStatus.InvalidCode:
                        var errors = <ErrorList>results.GetData();
                        this.errorDisplay.ShowErrors(errors);
                        this.exitMessage = "failed";
                        this.initiatedDeferred.reject(results);
                        break;
                    case ParsingStatus.InternalError:
                        console.log("Parser failed with: " + JSON.stringify(results.GetData()));
                        this.initiatedDeferred.reject(results);
                        break;
                }
            })
            .always(() => {
                this.ongoingParserPromise = undefined;
                this.initiatedDeferred = undefined;
            });
        return this.initiatedDeferred;
    }

    public Abort() {
        this.ongoingParserPromise.reject();
    }
}

