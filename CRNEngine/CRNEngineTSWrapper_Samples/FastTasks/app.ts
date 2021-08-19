import * as Interfaces from "../../CRNEngineTSWrapper/Scripts/Interfaces";
import CRNEngine from "../../CRNEngineTSWrapper/Scripts/CRNEngine";
import "../samples.css";
import "./styles.css";

var expressionsCodeBox = <HTMLTextAreaElement>document.getElementById("expressionsCodeBox");
var expressionsStatus = document.getElementById("expressionsStatus");

var me = new CRNEngine(false);

function appendExpressionsStatus(text:string) {
    var p = document.createElement("p");
    p.appendChild(document.createTextNode(text));
    expressionsStatus.appendChild(p);
}

function clearExpressionsStatus() {
    while (expressionsStatus.firstChild)
        expressionsStatus.removeChild(expressionsStatus.firstChild);
}

function expressionsParseClicked() {
    clearExpressionsStatus();
    appendExpressionsStatus("start");
    var code = expressionsCodeBox.value;
    var obs = me.ParseExpression(code);
    obs.subscribe(Rx.Observer.create<Interfaces.Expression>(v => appendExpressionsStatus("success (" + JSON.stringify(v) + ")"), (e: Interfaces.Error) => appendExpressionsStatus("fail (" + e.message + ")")));
}

document.getElementById("expressionsParseButton").onclick = expressionsParseClicked;