import * as SimulateOperation from '../Operations/SimulateParsedCRN';
import * as I from '../../../GenericComponents/Scripts/Interfaces';
import CRNEngine from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import * as MeInterfaces from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as ViewerFramework from '../../../../HTML5SharedGUI/SimulationViewer/Scripts/SimulationViewerFramework';
import * as PMViewer from '../../../../HTML5SharedGUI/SimulationViewer/Scripts/ProbabilityMapView';
import { Settings as ISpatialViewerSettings } from "../../../../HTML5SharedGUI/SimulationViewer/Scripts/SpatialViewerSettings";

// Adapts ProbabilityMapsViewer shared lib to the need of Simulation feature
export class Viewer implements SimulateOperation.ISimulationViewer<ViewerFramework.IVisualizationUpdateMessage>, PMViewer.IProbabilityMapProvider, I.IUIBindable {
    constructor(public crnEngine: CRNEngine, private settings: ISpatialViewerSettings) { }

    // Features.IUIBindable implementation
    public Bind(elem: HTMLElement) {
        this.viewer.Bind(elem);
    }

    private ConvertProbabilityMap(map: MeInterfaces.ProbabilityMap): PMViewer.ProbabilityMap {
        var pm = new PMViewer.ProbabilityMap();
        pm.times = map.times;
        pm.values = map.values;
        pm.probabilities = map.probabilities;
        return pm;
    }

    // PMViewer.IProbabilityMapProvider implementation
    public GetProbabilityMap(token: any, species: string, lowerBound: number): Rx.Observable<PMViewer.ProbabilityMap> {
        var that = this;
        return this.crnEngine.GetProbabilityMap(<MeInterfaces.Probabilities>token, species, lowerBound)
            .select(that.ConvertProbabilityMap);
    }

    // SimulateOperation.ISimulationViewer<ViewerFramework.IVisualizationUpdateMessage> implementation
    public Display(visualization: Rx.Observable<ViewerFramework.IVisualizationUpdateMessage>) {
        var that = this;
        this.viewer.Reset();
        visualization.subscribeOnNext(vum => {
            if (vum.MessageType == ViewerFramework.VisualizationUpdateMessageType.Probabilities)
                that.viewer.onNext(vum);
            else if (vum.MessageType == ViewerFramework.VisualizationUpdateMessageType.PlotSettingsInfo)
                that.viewer.onNext(vum);
            else if (vum.MessageType == ViewerFramework.VisualizationUpdateMessageType.SimulationStep)
                that.viewer.onNext(vum);
            else if (vum.MessageType == ViewerFramework.VisualizationUpdateMessageType.TraceDefinitions)
                that.viewer.onNext(vum);
            else if (vum.MessageType == ViewerFramework.VisualizationUpdateMessageType.Reset)
                that.viewer.Reset();
        });
    }

    private viewer = new PMViewer.ProbabilityMapView(this, this.settings);
}