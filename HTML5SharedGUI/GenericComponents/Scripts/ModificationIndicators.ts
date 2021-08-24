import * as $ from 'jquery';

export interface IModificationIndicator {
    SetModified(): void;
    Clear(): void;
}

export class TabSuffixIndicator implements IModificationIndicator {
    /* The target here is the textual element where the indicator is to be displayed. */
    constructor(private target: JQuery) { }

    private isSet = false;
    public getIsSet() { return this.isSet; }
    private static marker = " *";

    public SetModified() {
        if (this.isSet)
            return;
        if (this.target.length == 0)
            console.log("No TabSuffixIndicator target for " + this.target);
        this.target.each((idx, elem) => {
            var text = elem.textContent;
            elem.textContent = text + TabSuffixIndicator.marker;
            this.isSet = true;
        });
    }

    public Clear() {
        if (!this.isSet)
            return;
        if (this.target.length == 0)
            console.log("No TabSuffixIndicator target for " + this.target);
        this.target.each((idx, elem) => {
            var text = elem.textContent;
            var len = text.length;
            elem.textContent = text.substring(0, len - TabSuffixIndicator.marker.length);
        });
        this.isSet = false;
    }
}