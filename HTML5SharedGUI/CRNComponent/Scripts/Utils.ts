// Copy the element tree to the SVG tree. Assigning the element directly does not work.
export function copyTree(source: any, dest: Element) {
    if (source.tagName === undefined) {
        // It's a text node. Copy it.
        var txt = document.createTextNode(source.textContent);
        dest.appendChild(txt);
    }
    else {
        // Make a new tag with the same type.
        var el = document.createElementNS("http://www.w3.org/2000/svg", source.tagName);
        // Copy all attributes.
        for (var i = 0; i < source.attributes.length; i++)
            el.setAttribute(source.attributes[i].name, source.attributes[i].value);
        // Recurse on children.
        for (var i = 0; i < source.childNodes.length; i++) {
            var child = <Element>source.childNodes[i];
            copyTree(child, el);
        }
        dest.appendChild(el);
    }
}