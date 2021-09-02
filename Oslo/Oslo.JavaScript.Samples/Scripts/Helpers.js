// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

var hist = function (data) {
    var binsCount = 50;
    var len = data.length;
    var min, max;
    min = max = data[0];
    for (var i = 1; i < len; i++) {
        min = Math.min(min, data[i]);
        max = Math.max(max, data[i]);
    }
    var step = (max - min) / binsCount;
    var hist = new Array(binsCount);
    for (var i = 0; i < len; i++) {
        var elem = data[i];
        var idx;
        if (elem == max)
            idx = binsCount - 1;
        else
            idx = Math.floor((elem - min) / step);
        if (hist[idx] === undefined)
            hist[idx] = 0;
        hist[idx] += 1
    }
    //normalizing
    var x = Array(binsCount);
    for (var i = 0; i < binsCount; i++) {
        hist[i] /= len * step;
        x[i] = i * step + step * 0.5;
    }
    return {
        x: x,
        y: hist
    }
}

var slice = function (table, columnNum) {
    var result = Array(table.length);
    for (var i = 0; i < table.length; i++)
        result[i] = table[i][columnNum];
    return result;
}
