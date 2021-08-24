import * as SimulateOperation from '../Operations/SimulateParsedCRN';
import * as I from '../../../GenericComponents/Scripts/Interfaces';
import * as serialization from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as ViewerFramework from '../../../../HTML5SharedGUI/SimulationViewer/Scripts/SimulationViewerFramework';
import * as SimViewer from '../../../../HTML5SharedGUI/SimulationViewer/Scripts/Viewer';

//Adapts SimViewer shared lib to the need of Simulation feature
export class Viewer implements SimulateOperation.ISimulationViewer<ViewerFramework.IVisualizationUpdateMessage>, I.IUIBindable {
    private viewer = new SimViewer.Viewer();

    //Features.IUIBindable implementation
    public Bind(elem: HTMLElement) {
        this.viewer.bind(elem);
    }

    public AutoBind(elem: HTMLElement) {
        this.viewer.autoBind(elem);
    }

    //SimulateOperation.ISimulationViewer<ViewerFramework.IVisualizationUpdateMessage> implementation
    public Display(visualization: Rx.Observable<ViewerFramework.IVisualizationUpdateMessage>) {
        var awaitingFirstSimulationMessage = true;
        var resetMessage: ViewerFramework.IVisualizationUpdateMessage = {
            MessageType: ViewerFramework.VisualizationUpdateMessageType.Reset,
            EncapsulatedUpdate: undefined
        };
        visualization.subscribe(
            <any>{
                onNext: (vum: ViewerFramework.IVisualizationUpdateMessage) => {
                    if (awaitingFirstSimulationMessage) {
                        awaitingFirstSimulationMessage = false;
                        this.viewer.Post(resetMessage);
                    }
                    this.viewer.Post(vum);
                },
                onError: (error: any) => { console.error("Sim viewer adapter got error in the visualization update message stream: " + JSON.stringify(error)); },
                onCompleted: () => {
                    awaitingFirstSimulationMessage = true;
                }
            });
    }
}