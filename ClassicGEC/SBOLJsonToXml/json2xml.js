var fs = require('fs');
var SBOLDocument = require('sboljs');

var sboljsonFilepath = 'sbol.json'
var resultSBOL = 'gec.xml';
var sbol = new SBOLDocument();

var sbolJson = JSON.parse(fs.readFileSync(sboljsonFilepath) + '');

var seqMap = {};
var compMap = {};
var funCompMap = {};
convertJsonToXml(sbolJson)

//console.log(seqMap);
//console.log(compMap);
//console.log(funCompMap);
//console.log(sbol);

//Write the SBOL document to an XML file.
fs.writeFile(resultSBOL, sbol.serializeXML(), function (err) {
    if (err) {
        return console.log(err);
    }

    console.log("GEC SBOL XML file created!");
});


function convertJsonToXml(jsbol){
    jsbol.sequences.forEach(function(element) {
        addSequence(element);
    }, this);
    jsbol.componentDefinitions.forEach(function(element) {
        addComponentDefinition(element);
    }, this);
    jsbol.moduleDefinitions.forEach(function(element) {
        addModuleDefinition(element);
    }, this);
}

function addSequence(jelem){
    var elem = sbol.sequence();
    //Start adding basic information like identity
    elem.name = jelem.name;
    elem.version = jelem.version;
    elem.displayId = jelem.displayId;
    elem.persistentIdentity = encodeURI(jelem.persistentIdentity);
    elem.uri =  elem.persistentIdentity + '/' + elem.version;
    if(jelem.description.length !== 0){
        elem.description = jelem.description;
    }
    jelem.annotations.forEach(function (ann){
        if(ann.Type === "uri"){
            elem.addUriAnnotation(ann.name,ann.value);
        }
        else if(ann.Type === "string"){
            elem.addStringAnnotation(ann.name,ann.value);
        }
    },this);
    //End basic information
    elem.elements = jelem.elements;
    elem.encoding = jelem.encoding;

    let uriVal = jelem.uri
    seqMap[uriVal] = elem;

}

function addComponentDefinition(jelem){
    
    var elem = sbol.componentDefinition();
    //Start adding basic information like identity
    elem.name = jelem.name;
    elem.version = jelem.version;
    elem.displayId = jelem.displayId;
    elem.persistentIdentity = encodeURI(jelem.persistentIdentity);
    elem.uri =  elem.persistentIdentity + '/' + elem.version;
    if(jelem.description.length !== 0){
        elem.description = jelem.description;
    }
    jelem.annotations.forEach(function (ann){
        if(ann.Type === "uri"){
            elem.addUriAnnotation(ann.name,ann.value);
        }
        else if(ann.Type === "string"){
            elem.addStringAnnotation(ann.name,ann.value);
        }
    },this);
    //End basic information

    jelem.sequences.forEach(function (sequri){
        elem.addSequence(seqMap[sequri]);
    },this);

    jelem.components.forEach(function(comp){
        elem.addComponent(addComponent(comp));
    },this);
    jelem.sequenceAnnotations.forEach(function(sa){
        elem.addSequenceAnnotation(addSequenceAnnotation(sa));
    },this);

    jelem.types.forEach(function(t){
        elem.addType(t);
    },this);

    jelem.roles.forEach(function(r){
        elem.addRole(r);
    },this);
    return elem;
}

function addSequenceAnnotation(jelem){
    var elem = sbol.sequenceAnnotation();
    //Start adding basic information like identity
    elem.name = jelem.name;
    elem.version = jelem.version;
    elem.displayId = jelem.displayId;
    elem.persistentIdentity = encodeURI(jelem.persistentIdentity);
    elem.uri =  elem.persistentIdentity + '/' + elem.version;
    if(jelem.description.length !== 0){
        elem.description = jelem.description;
    }
    jelem.annotations.forEach(function (ann){
        if(ann.Type === "uri"){
            elem.addUriAnnotation(ann.name,ann.value);
        }
        else if(ann.Type === "string"){
            elem.addStringAnnotation(ann.name,ann.value);
        }
    },this);
    //End basic information

    elem.component = compMap[jelem.Component]; 
    jelem.locations.forEach(function(loc){
        elem.addLocation(addLocation(loc));
    },this);
    return elem;
}

function addLocation(jloc){
    var suff = "$" + jloc.$;
    if(jloc[suff].gecDU === "range"){
        var jelem = jloc[suff];
        var elem = sbol.range();
        //Start adding basic information like identity
        elem.name = jelem.name;
        elem.version = jelem.version;
        elem.displayId = jelem.displayId;
        elem.persistentIdentity = encodeURI(jelem.persistentIdentity);
        elem.uri =  elem.persistentIdentity + '/' + elem.version;
        if(jelem.description.length !== 0){
            elem.description = jelem.description;
        }
        jelem.annotations.forEach(function (ann){
            if(ann.Type === "uri"){
                elem.addUriAnnotation(ann.name,ann.value);
            }
            else if(ann.Type === "string"){
                elem.addStringAnnotation(ann.name,ann.value);
            }
        },this);
        //End basic information
        elem.start = jelem.startIndex;
        elem.end = jelem.endIndex;
        elem.orientation = jelem.orientation;
        return elem;
    }    
    
}

function addComponent(jelem){
    var elem = sbol.component();
    //Start adding basic information like identity
    elem.name = jelem.name;
    elem.version = jelem.version;
    elem.displayId = jelem.displayId;
    elem.persistentIdentity = encodeURI(jelem.persistentIdentity);
    elem.uri =  elem.persistentIdentity + '/' + elem.version;
    if(jelem.description.length !== 0){
        elem.description = jelem.description;
    }
    jelem.annotations.forEach(function (ann){
        if(ann.Type === "uri"){
            elem.addUriAnnotation(ann.name,ann.value);
        }
        else if(ann.Type === "string"){
            elem.addStringAnnotation(ann.name,ann.value);
        }
    },this);
    //End basic information
    elem.access = jelem.access;
    elem.definition = jelem.definition;
    let uriVal = jelem.uri
    compMap[uriVal] = elem;
    return elem;
}


function addModuleDefinition(jelem){
    var elem = sbol.moduleDefinition();
    //Start adding basic information like identity
    elem.name = jelem.name;
    elem.version = jelem.version;
    elem.displayId = jelem.displayId;
    elem.persistentIdentity = encodeURI(jelem.persistentIdentity);
    elem.uri =  elem.persistentIdentity + '/' + elem.version;
    if(jelem.description.length !== 0){
        elem.description = jelem.description;
    }
    jelem.annotations.forEach(function (ann){
        if(ann.Type === "uri"){
            elem.addUriAnnotation(ann.name,ann.value);
        }
        else if(ann.Type === "string"){
            elem.addStringAnnotation(ann.name,ann.value);
        }
    },this);
    //End basic information

    jelem.functionalComponents.forEach(function (fc){
        elem.addFunctionalComponent(addFunctionalComponent(fc));
    },this);
    jelem.interactions.forEach(function (inter){
        elem.addInteraction(addInteraction(inter));
    },this);
    return elem;
}


function addFunctionalComponent(jelem){
    var elem = sbol.functionalComponent();
    //Start adding basic information like identity
    elem.name = jelem.name;
    elem.version = jelem.version;
    elem.displayId = jelem.displayId;
    elem.persistentIdentity = encodeURI(jelem.persistentIdentity);
    elem.uri =  elem.persistentIdentity + '/' + elem.version;
    if(jelem.description.length !== 0){
        elem.description = jelem.description;
    }
    jelem.annotations.forEach(function (ann){
        if(ann.Type === "uri"){
            elem.addUriAnnotation(ann.name,ann.value);
        }
        else if(ann.Type === "string"){
            elem.addStringAnnotation(ann.name,ann.value);
        }
    },this);
    //End basic information
    elem.access = jelem.access;
    elem.direction = jelem.direction;
    elem.definition = jelem.definition; 
    let uriVal = jelem.uri
    funCompMap[uriVal] = elem;  
    return elem;
}


function addParticipation(jelem){
    var elem = sbol.participation();
    //Start adding basic information like identity
    elem.name = jelem.name;
    elem.version = jelem.version;
    elem.displayId = jelem.displayId;
    elem.persistentIdentity = encodeURI(jelem.persistentIdentity);
    elem.uri =  elem.persistentIdentity + '/' + elem.version;
    if(jelem.description.length !== 0){
        elem.description = jelem.description;
    }
    
    jelem.annotations.forEach(function (ann){
        if(ann.Type === "uri"){
            elem.addUriAnnotation(ann.name,ann.value);
        }
        else if(ann.Type === "string"){
            elem.addStringAnnotation(ann.name,ann.value);
        }
    },this);
    
    //End basic information
    jelem.roles.forEach(function(r) {
        elem.addRole(r);
    },this);

    elem.participant = funCompMap[jelem.participant];
    return elem;
}

function addInteraction(jelem){
    var elem = sbol.interaction();
    //Start adding basic information like identity
    elem.name = jelem.name;
    elem.version = jelem.version;
    elem.displayId = jelem.displayId;
    elem.persistentIdentity = encodeURI(jelem.persistentIdentity);
    elem.uri =  elem.persistentIdentity + '/' + elem.version;
    if(jelem.description.length !== 0){
        elem.description = jelem.description;
    }
    jelem.annotations.forEach(function (ann){
        if(ann.Type === "uri"){
            elem.addUriAnnotation(ann.name,ann.value);
        }
        else if(ann.Type === "string"){
            elem.addStringAnnotation(ann.name,ann.value);
        }
    },this);
    //End basic information

    jelem.types.forEach(function(t){
        elem.addType(t);
    },this);
    
    jelem.participations.forEach(function(part){
        elem.addParticipation(addParticipation(part));
    },this);
    return elem;
}