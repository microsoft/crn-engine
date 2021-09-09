// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as CRNInterfaces from "./../../../CRNEngine/CRNEngineTSWrapper/Scripts/InternalInterfaces";
import * as Interfaces from "./Interfaces";
/*import * as sboljs from 'sboljs';

export function jsbolToObject(jsbol: Interfaces.jSBOLDocument) {
    var sboldoc = new sboljs()
    jsbol.componentDefinitions.forEach(cd => {
        var compdef = sboldoc.componentDefinition();
        compdef.name = cd.name
        cd.roles.forEach(role => compdef.addRole(role));
        cd.types.forEach(t => compdef.addType(t));
        compdef.version = cd.version
        compdef.uri = cd.uri
        compdef.persistentIdentity = cd.persistentIdentity
        compdef.displayId = cd.displayId
        cd.components.forEach(comp => {
            var component = sboldoc.component();

            component.name = comp.name;
            component.displayId = comp.displayId;
            component.persistentIdentity = comp.persistentIdentity;
            component.version = comp.version;
            component.uri = comp.uri;
            component.definition = comp.definition;
            component.access = comp.access;

            compdef.addComponent(component);
        });

        cd.sequenceAnnotations.forEach(sa => {
            var seqann = sboldoc.sequenceAnnotation();
            seqann.name = sa.name;
            seqann.displayId = sa.displayId;
            seqann.persistentIdentity = sa.persistentIdentity;
            seqann.version = sa.version;
            seqann.uri = sa.uri;

            sa.ranges.forEach(range => {
                var r = sboldoc.range();
                r.name = range.name;
                r.displayId = range.displayId;
                r.persistentIdentity = range.persistentIdentity;
                r.version = range.version;
                r.uri = range.uri;
                r.start = range.startIndex;
                r.end = range.endIndex;
                r.orientation = range.orientation;
                seqann.addLocation(r);
            });

            sa.roles.forEach(role => seqann.addRole(role));

            compdef.addSequenceAnnotation(seqann);

        });
    });
    return sboldoc;
}

export function jsbolToExport(jsbol: Interfaces.jSBOLDocument) {
    var sboldoc = jsbolToObject(jsbol);
    var sbolxmlstring = sboldoc.serializeXML();
    var exportmsg: CRNInterfaces.WorkerResponse_Export = { mtype: "export", export: { "content_type": "text/plain", id: "sbol", display_name: "SBOL", content: sbolxmlstring } };
    return exportmsg;
}
*/