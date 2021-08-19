import * as $ from 'jquery';
import * as crnVM from "../../../../HTML5SharedGUI/CRNComponent/Scripts/crnVM";
import * as serialization from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as HintScreen from "../../../GenericComponents/Scripts/HintScreen";
import * as Operations from "./LongOperations";
import * as GenericParsing from "./GenericCodeParsing";

//Dependencies to be injected

export interface IParsed<TCustomSettings> {
    Model: serialization.IG,
    CustomSettings: TCustomSettings,
}

/** Displays the CRN entities within a GUI */
export interface IModelViewer<TCustomSettings> {
    /** Updates the view with data from the supplied CRN. Set fromJIT to true if this CRN comes as the result of completing a JIT operation. This may affect which of the underlying VMs get updated. */
    UpdateValuesWith(update: serialization.IG, customSettings: TCustomSettings, fromJIT: boolean): void;
}

/**
 * Gets the code from the CodeEditor, passes it to the parser, receives CRN entities, updates CRN viewer with them, updates data files selecting control with parsed "data directive" files.
 * If any parsing error occurs, displays it with errorDisplay
 */
export class Operation<TOptions, TCustomSettings> extends GenericParsing.Operation<IParsed<TCustomSettings>, TOptions> implements Operations.IOperation, HintScreen.IHintRemoveNotifier {
    constructor(
        codeSource: GenericParsing.ICodeSource<TOptions>,
        parser: GenericParsing.IParser<IParsed<TCustomSettings>, TOptions>,
        private modelViewer: IModelViewer<TCustomSettings>,
        errorDisplay: GenericParsing.IErrorDisplay) {
        super(codeSource, parser, errorDisplay);
    }

    //defining abstract functions
    public GetName() {
        return "CRN code parsing";
    }

    protected InterpretSuccessfulResults(data: IParsed<TCustomSettings>) {
        this.modelViewer.UpdateValuesWith(data.Model, data.CustomSettings, false);
        this.notificationCallbacks.forEach(c => { c() }); // notifing that hints now can be removed
    }

    //HintScreen.IHintRemoveNotifier implementation
    private notificationCallbacks: Array<() => void> = [];
    public SubscribeRemoveHint(callback: () => void) {
        this.notificationCallbacks.push(callback);
    }
}