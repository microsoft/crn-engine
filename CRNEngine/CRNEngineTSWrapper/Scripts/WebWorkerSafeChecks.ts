//PhantomJS which is used in build agent test environment does not have console. Placing a stub here, so the scripts calling console won't crash
if (typeof console === "undefined")
    console = <any>{
        assert: (test?: boolean, message?: string, ...optionalParams: any[]) => { throw "Assertion not supported";},
        clear: () => { },
        count: (countTitle?: string) => { },
        debug: (message?: string, ...optionalParams: any[]) => { },
        dir: (value?: any, ...optionalParams: any[]) => { },
        dirxml: (value: any) => { },
        error: (message?: any, ...optionalParams: any[]) => { },
        group: (groupTitle?: string) => { },
        groupCollapsed: (groupTitle?: string) => { },
        groupEnd: () => { },
        info: (message?: any, ...optionalParams: any[]) => { },
        log: (message?: any, ...optionalParams: any[]) => { },
        msIsIndependentlyComposed: (element: Element) => { return false; },
        profile: (reportName?: string) => { },
        profileEnd: () => { },
        select: (element: Element) => { },
        time: (timerName?: string) => { },
        timeEnd: (timerName?: string) => { },
        trace: () => { },
        warn: (message?: any, ...optionalParams: any[]) => { },
    };