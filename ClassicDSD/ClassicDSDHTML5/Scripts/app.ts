import { Launch as Bootstrap } from '../../../HTML5SharedGUI/HTML5DSD_Generic/Scripts/GenericApp';
import { Parser as NotExpandingParser } from './Adapters/DSDCRNUnexpandedParser';
import { Parser as ExpandingParser } from './Adapters/DSDCRNExpandedParser';
import { INamedMonarchLanguage } from '../../../HTML5SharedGUI/CodeEditor/Scripts/CodeEditor';
import * as CrnEditor from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedCRNViewer';
import DSDLanguage from './DSDLang';
import DSD from '../../ClassicDSDTSWrapper/Scripts/ClassicDSD';
import Options from './DSDParsingOptions';
import * as ko from 'knockout';
import SequencesDatabase from './SequencesDatabase/SequencesDatabase';
import DSDDirectives from './DSDDirectives';
import { DsdSettings as DsdSettings } from '../../ClassicDSDTSWrapper/Scripts/Interfaces';
import * as optionsTemplate from "raw-loader!../Templates/options_template.html";
import CRNEditor from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Components/CRNCodeEditor';
import * as CRNVM from '../../../HTML5SharedGUI/CRNComponent/Scripts/crnVM';

var path = "./Examples/DSDModels/";
var lpPath = path + "RulesDSD/"
var examples: ExamplesGroup[] = [
    //{
    //  Name: "Group 1",
    //  Correspondence: { "Catalytic"                                      : path+"Catalytic.txt"
    //                  , "Catalytic with sequence-based rate calculations": path+"CatalyticSequenceRates.txt"
    //                  , "3-stator transmission line"                     : path+"TransmissionLines.txt"
    //  }
    //}
    {
        Name: "Manual",
        Correspondence: {
              "Join": path + "Manual/Join.txt"
            , "Join - Inference": path + "Manual/Join-Inference.txt"
            , "Join - Inference - Samples": path + "Manual/Join-Inference-Samples.txt"
            , "Join - CTMC": path + "Manual/Join-CTMC.txt"
            , "Join - Initials": path + "Manual/Join-Initials.txt"
            , "Join - Stochastic": path + "Manual/Join-Stochastic.txt"
            , "Join - LNA": path + "Manual/Join-LNA.txt"
            , "Join - CME": path + "Manual/Join-CME.txt"
            , "Join - Spatial": path + "Manual/Join-Spatial.txt"
            , "Join - Nucleotides": path + "Manual/Join-Nucleotides.txt"
            , "Join - Reactions": path + "Manual/Join-Reactions.txt"
            , "Join - Subdomains": path + "Manual/Join-Subdomains.txt"
            , "Join - Unique": path + "Manual/Join-Unique.txt"
            , "Join - Localised": path + "Manual/Join-Localised.txt"
            , "Join - JIT": path + "Manual/Join-JIT.txt"
            , "Join - Leaks": path + "Manual/Join-Leaks.txt"
        }
    }
    ,
    {
        Name: "Spaccasassi et al., (ACS Synth Biol 2018)",
        Correspondence: {
              "Join - Rules": lpPath + "join_simple.txt"
            , "Join - infinite semantics": lpPath + "join_infinite.txt"
            , "Join - Kinetic rates": lpPath + "join_kinetic_rates.txt"
            , "Catalytic - Rules": lpPath + "catalytic.txt"
            , "Enzymes": lpPath + "enzymes.txt"
            , "Oligator": lpPath + "oligator.txt"
            , "Oligator nucleotides": lpPath + "oligator_nucleotides.txt"
            , "Ribocomputing AND gate": lpPath + "ribocomputing_AND.txt"
            , "Ribocomputing OR gate": lpPath + "ribocomputing_OR.txt"
            , "Localized HCR": lpPath + "localized_hcr.txt"
            , "Cargo-sorting DNA robot": lpPath + "cargo_sorting_DNA_robot.txt"
            , "Enzymatic walker": lpPath + "walker_enzymatic.txt"
        }
    }
    ,
    {
        Name: "Catalytic (Science 2007)",
        Correspondence: {
              "Catalytic": path + "Scienze 2007 - Catalytic.txt"
            , "Catalytic Directives": path + "Scienze 2007 - Catalytic Directives.txt"
        }
    }
    ,
    {
        Name: "Yordanov et al. (ACS Synth Biol 2014)",
        Correspondence: {
              "PI Controller - 2Domain": path + "Yordanov et al., ACS Synth Biol 2014 - PI Controller - 2Domain.txt"
            , "PI Controller - 4Domain": path + "Yordanov et al., ACS Synth Biol 2014 - PI Controller - 4Domain.txt"
            , "PI Controller - DNA Enzymes": path + "Yordanov et al., ACS Synth Biol 2014 - PI Controller - DNA Enzymes.txt"
            , "PI Controller - RNA Enzymes": path + "Yordanov et al., ACS Synth Biol 2014 - PI Controller - RNA Enzymes.txt"
            , "PI Controller - CRN": path + "Yordanov et al., ACS Synth Biol 2014 - PI Controller - CRN.txt"
        }
    }
    ,
    {
        Name: "Kim et al. (MSB 2011)",
        Correspondence: { "Oscillator - Kim": path + "Kim et al., Mol Syst Biol 2011 - Oscillator.txt" }
    }
    ,
    {
        Name: "Montagne et al. (MSB 2011)",
        Correspondence: { "Oscillator - Montagne": path + "Montagne et al., Mol Syst Biol 2011 - Oscillator.txt" }
    }
    ,
    {
        Name: "Lakin et al. (DNA 2014)",
        Correspondence: {
              "Transmission lines": path + "Lakin et al., DNA 2014 - Transmission lines.txt"
            , "Remote transmission lines": path + "Lakin et al., DNA 2014 - Remote transmission lines.txt"
            , "Threshold-based spatial AND": path + "Lakin et al., DNA 2014 - Threshold spatial AND.txt"
            , "Threshold-based spatial AND (reporter)": path + "Lakin et al., DNA 2014 - Threshold spatial (reporter) AND.txt"
        }
    }
    ,
    {
        Name: "Lakin, Parker et al. (Interface 2012)",
        Correspondence: { "Model-checking examples": path + "Lakin, Parker et al., Interface 2012 - Model-checking examples.txt" }
    }
    ,
    {
        Name: "Lakin et al. (Interface 2012)",
        Correspondence: {
              "Join gates": path + "Lakin et al., Interface 2012 - Join gates.txt"
            , "Buffered oscillators": path + "Lakin et al., Interface 2012 - Buffered oscillators.txt"
            , "Unbuffered oscillators": path + "Lakin et al., Interface 2012 - Unbuffered oscillators.txt"
        }
    }
    ,
    {
        Name: "Co-operative",
        Correspondence: {
              "Lotka": path + "Co-operative - Lotka.txt"
            , "Mapk": path + "Co-operative - Mapk.txt"
        }
    }
    ,
    {
        Name: "Simple",
        Correspondence: {
              "AND Circuit": path + "Simple - AND Circuit.txt"
            , "Migrations": path + "Simple - Migrations.txt"
            , "Monomers": path + "Simple - Monomers.txt"
            , "Hairpin-free HCR ": path + "Simple - Hairpin-free HCR.txt"
        }
    }
    ,
    {
        Name: "3-domain (DNA 2010)",
        Correspondence: {
              "Transducer": path + "3-domain, DNA 2016 - Transducer.txt"
            , "Transducer Composition": path + "3-domain, DNA 2016 - Transducer Composition.txt"
            , "Buffered Transducer": path + "3-domain, DNA 2016 - Buffered Transducer.txt"
            , "Buffered Fork": path + "3-domain, DNA 2016 - Buffered Fork.txt"
            , "Buffered Join": path + "3-domain, DNA 2016 - Buffered Join.txt"
            , "Oscillating": path + "3-domain, DNA 2016 - Oscillating.txt"
            , "Ultrasensitive": path + "3-domain, DNA 2016 - Ultrasensitive.txt"
        }
    }
    ,
    {
        Name: "2-domain (DCM)",
        Correspondence: {
              "Two-domain transducer": path + "2-domain, DCM - Two-domain transducer.txt"
            , "Two-domain fork/join": path + "2-domain, DCM - Two domain fork-join.txt"
        }
    }
    ,
    {
        Name: "Stack machine (DNA 2011)",
        Correspondence: {
            "Ripple carry adder": path + "Stack Machine, DNA17 - Ripple carry adder.txt"
        }
    }
    ,
    {
        Name: "Zhang (Science 2007)",
        Correspondence: {
              "Fluorescent Catalytic": path + "Zhang, Science 2007 - Fluorescent Catalytic.txt"
            , "Autocatalytic": path + "Zhang, Science 2007 - Autocatalytic.txt"
            , "Feed Forward": path + "Zhang, Science 2007 - Feed forward.txt"
            , "Cross Catalytic": path + "Zhang, Science 2007 - Cross Catalytic.txt"
            , "And Gate": path + "Zhang, Science 2007 - And Gate.txt"
            , "Independent Input": path + "Zhang, Science 2007 - Independent Input.txt"
        }
    }
    ,
    {
        Name: "Square root",
        Correspondence: { "Qian and Winfree 4 bit square root circuit": path + "Square root - Qian and Winfree 4 bit square root circuit.txt" }
    }
    ,
    {
        Name: "Localization",
        Correspondence: {
              "OR gate": path + "Localization - OR gate.txt"
            , "OR gate with nicks": path + "Localization - OR gate with nicks.txt"
            , "AND gate": path + "Localization - AND gate.txt"
            , "AND gate with nicks": path + "Localization - AND gate with nicks.txt"
            , "Wires": path + "Localization - Wires.txt"
            , "OR of ANDs": path + "Localization - OR of ANDs.txt"
            , "Square root": path + "Localization - Square root.txt"
        }
    }
    ,
    {
        Name: "Consensus (Nature Nanotechnology 2013)",
        Correspondence: {
              "Consensus Modules": path + "Nature Nanotech 2013 - Consensus Modules.txt"
            , "Consensus Parameterized": path + "Nature Nanotech 2013 - Consensus Parameterized.txt"
        }
    }
    ,
    {
        Name: "Formal Analysis",
        Correspondence: {
              "Fanout (localized)": path + "Formal Analysis - (localized) Fanout.txt"
            , "Fanout": path + "Formal Analysis - Fanout.txt"
            , "And": path + "Formal Analysis - And.txt"
            , "Or": path + "Formal Analysis - Or.txt"
            , "SQRT (localized)": path + "Formal Analysis - (localized) SQRT.txt"
            , "SQRT (localized Fanout)": path + "Formal Analysis - (localized Fanout) SQRT.txt"
            , "SQRT": path + "Formal Analysis - SQRT.txt"
        }
    }
];

var engine = new DSD(Options.Server());

var expandingParser = new ExpandingParser(engine);
var notExpandingParser = new NotExpandingParser(engine);
var options = new Options(engine);
var dsdSettings: CRNVM.ExternalSetting = null;

var customSettingsConverter = {
    convert: function (owner: CRNVM.CRN, settings: DsdSettings): CRNVM.ExternalSetting[] {
        var ret = new DSDDirectives(owner);
        ret.source = settings;
        return [ret];
    },
    convertBack: function (src: CRNVM.CRN): DsdSettings {
        if (src.externalSettings().length > 0)
            return (<DSDDirectives>src.externalSettings()[0]).source;
        return null;
    }
};

Bootstrap(<INamedMonarchLanguage>DSDLanguage, examples, notExpandingParser, expandingParser, engine, options, customSettingsConverter);

function addOptions() {
    var optionsContainer = document.getElementById("options");
    optionsContainer.innerHTML = optionsTemplate;
    var $_elem = $(optionsContainer);
    $('ul.dropdown_menu li ul', $_elem).hide();
    var $li = $('ul.dropdown_menu li', $_elem);
    $li.focusout(function (this: JQuery) {
        //$('ul', this).stop().slideUp(200);
    });
    $li.focusin(function (this: JQuery) {
        $('ul', this).stop().slideDown(200);
    });
    $li.hover(function (this: JQuery) {
        $('ul', this).stop().slideDown(200);
    },
        function (this: JQuery) {
            // If there are any text inputs within the control, then I don't want to close it just because the user moves the mouse away.
            var focused = $("*:focus", this);
            for (let i = 0; i < focused.length; i++)
                if (focused[i].tagName.toLowerCase() == "input" && ((<HTMLInputElement>focused[i]).type == "text"))
                    return;
            $('ul', this).stop().slideUp(200);
        });
    /*var toolbar = document.getElementById("toolbar");
    var executionStatus = document.getElementById("execution-status");
    var oldSyntaxCheckbox = document.createElement("input");
    oldSyntaxCheckbox.type = "checkbox";
    oldSyntaxCheckbox.setAttribute("data-bind", "checked: oldSyntax");
    toolbar.insertBefore(oldSyntaxCheckbox, executionStatus);
    toolbar.insertBefore(document.createTextNode("old syntax"), executionStatus);*/
    ko.applyBindings(options, optionsContainer);
}

function addSequencesDatabase() {
    var containerTab = document.getElementById("libraryTab");
    var sequencesDatabaseListItem = document.createElement("li");
    var sequencesDatabaseAnchor = document.createElement("a");
    sequencesDatabaseAnchor.href = "#sequencesDatabase";
    sequencesDatabaseAnchor.appendChild(document.createTextNode("Sequences"));
    sequencesDatabaseListItem.appendChild(sequencesDatabaseAnchor);
    var tabList = $("#libraryTab>ul")[0];
    tabList.insertBefore(sequencesDatabaseListItem, tabList.firstChild);
    var sequencesDiv = document.createElement("div");
    sequencesDiv.setAttribute("class", "tabPanel");
    sequencesDiv.setAttribute("id", "sequencesDatabase");
    containerTab.insertBefore(sequencesDiv, document.getElementById("dataTab"));
    $("#libraryTab").tabs("refresh");
    SequencesDatabase(sequencesDiv, engine);
}

addOptions();
addSequencesDatabase();