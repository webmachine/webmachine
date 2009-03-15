var HIGHLIGHT = '#cc00cc';
var REGULAR = '#666666';

var cols = {
    'a':173,
    'b':325,
    'c':589,
    'd':797,
    'e':1005,
    'f':1195,
    'g':1402,
    'gg':1515,
    'h':1572,
    'i':1799,
    'j':1893,
    'k':1988,
    'l':2157,
    'll':2346,
    'm':2403,
    'mm':2535,
    'n':2554,
    'o':2649,
    'oo':2781,
    'ooo':2801,
    'p':2894,
    'q':3007
};

var rows = {
    '1':221,
    '2':298,
    '3':373,
    '4':448,
    '5':524,
    '6':599,
    '7':675,
    '8':751,
    '9':826,
    '10':902,
    '11':977,
    '12':1053,
    '13':1129,
    '14':1204,
    '15':1280,
    '16':1355,
    '17':1431,
    '18':1506,
    '19':1583,
    '20':1658,
    '21':1734,
    '22':1809,
    '23':1885,
    '24':1961,
    '25':2036,
    '26':2112
};

var edges = {
    'b14b13':['b14','b13'],

    'b13b12':['b13','b12'],
    'b13503':['b13','503'],

    'b12b11':['b12','b11'],
    'b12501':['b12','501'],

    'b11b10':['b11','b10'],
    'b11414':['b11','414'],

    'b10b9':['b10','b9'],
    'b10405':['b10','405'],

    'b9b8':['b9','b8'],
    'b9400':['b9','400'],

    'b8b7':['b8','b7'],
    'b8401':['b8','401'],

    'b7b6':['b7','b6'],
    'b7403':['b7','403'],

    'b6b5':['b6','b5'],
    'b6501':['b6','501a'],

    'b5b4':['b5','b4'],
    'b5415':['b5','415'],

    'b4b3':['b4','b3'],
    'b4413':['b4','b4'],

    'b3c3':['b3','c3'],
    'b3200':['b3','200'],

    'c3c4':['c3','c4'],
    'c3d4':['c3','d3','d4'],

    'c4d4':['c4','d4'],
    'c4406':['c4','406'],

    'd4d5':['d4','d5'],
    'd4e5':['d4','e4','e5'],

    'd5e5':['d5','e5'],
    'd5406':['d5','d7','406'],

    'e5e6':['e5','e6'],
    'e5f6':['e5','f5','f6'],

    'e6f6':['e6','f6'],
    'e6406':['e6','e7','406'],

    'f6f7':['f6','f7'],
    'f6g7':['f6','g6','g7'],

    'f7g7':['f7','g7'],
    'f7406':['f7','406'],

    'g7g8':['g7','g8'],
    'g7h7':['g7','h7'],

    'g8g9':['g8','g9'],
    'g8h10':['g8','h8','h10'],

    'g9g11':['g9','g11'],
    'g9h10':['g9','gg9','gg10','h10'],

    'g11h10':['g11','gg11','gg10','h10'],
    'g11412':['g11','g18','412a'],

    'h7i7':['h7','i7'],
    'h7412':['h7','412'],

    'h10h11':['h10','h11'],
    'h10i12':['h10','i10','i12'],

    'h11h12':['h11','h12'],
    'h11i12':['h11','i11','i12'],

    'h12i12':['h12','i12'],
    'h12412':['h12','412a'],

    'i4p3':['i4','i3','p3'],
    'i4301':['i4','301'],

    'i7i4':['i7','i4'],
    'i7k7':['i7','k7'],

    'i12l13':['i12','l12','l13'],
    'i12i13':['i12','i13'],

    'i13k13':['i13','k13'],
    'i13j18':['i13','i17','j17','j18'],

    'j18412':['j18','412a'],
    'j18304':['j18','304'],

    'k5l5':['k5','l5'],
    'k5301':['k5','301'],

    'k7k5':['k7','k5'],
    'k7l7':['k7','l7'],

    'k13j18':['k13','k17','j17','j18'],
    'k13l13':['k13','l13'],

    'l5m5':['l5','m5'],
    'l5307':['l5','307'],

    'l7m7':['l7','m7'],
    'l7404':['l7','l8','404'],

    'l13l14':['l13','l14'],
    'l13m16':['l13','m13','m16'],

    'l14l15':['l14','l15'],
    'l14m16':['l14','m14','m16'],

    'l15l17':['l15','l17'],
    'l15m16':['l15','ll15','ll16','m16'],

    'l17m16':['l17','ll17','ll16','m16'],
    'l17304':['l17','304'],

    'm5n5':['m5','n5'],
    'm5410':['m5','m4','410'],

    'm7n11':['m7','n7','n11'],
    'm7404':['m7','404'],

    'm16m20':['m16','m20'],
    'm16n16':['m16','n16'],

    'm20o20':['m20','o20'],
    'm20202':['m20','202'],

    'n5n11':['n5','n11'],
    'n5410':['n5','410'],

    'n11p11':['n11','p11'],
    'n11303':['n11','303'],

    'n16n11':['n16','n11'],
    'n16o16':['n16','o16'],

    'o14p11':['o14','o11','p11'],
    'o14409':['o14','409a'],

    'o16o14':['o16','o14'],
    'o16o18':['o16','o18'],

    'o18200':['o18','200a'],
    'o18300':['o18','oo18','300'],

    'o20o18':['o20','o18'],
    'o20204':['o20','204'],

    'p3p11':['p3','p11'],
    'p3409':['p3','409'],

    'p11o20':['p11','p20','o20'],
    'p11201':['p11','q11','201']
};

var ends = {
    '200': {col:'a', row:'3', width:190},
    '200a': {col:'mm', row:'18', width:116},
    '201': {col:'q', row:'12', width:154},
    '202': {col:'m', row:'21', width:116},
    '204': {col:'o', row:'21', width:152},

    '300': {col:'oo', row:'19', width:152},
    '301': {col:'k', row:'4', width:154},
    '303': {col:'m', row:'11', width:116},
    '304': {col:'l', row:'18', width:116},
    '307': {col:'l', row:'4', width:154},

    '400': {col:'a', row:'9', width:190},
    '401': {col:'a', row:'8', width:190},
    '403': {col:'a', row:'7', width:190},
    '404': {col:'m', row:'8', width:116},
    '405': {col:'a', row:'10', width:190},
    '406': {col:'c', row:'7', width:152},
    '409': {col:'p', row:'2', width:116},
    '409a': {col:'oo', row:'14', width:116},
    '410': {col:'n', row:'4', width:116},
    '412': {col:'h', row:'6', width:152},
    '412a': {col:'h', row:'18', width:152},
    '413': {col:'a', row:'4', width:190},
    '414': {col:'a', row:'11', width:190},
    '415': {col:'a', row:'5', width:190},

    '501a': {col:'a', row:'6', width:190},
    '501': {col:'a', row:'12', width:190},
    '503': {col:'a', row:'13', width:190}
};

var canvas;
var detail;
var decisionId;
var decisionCalls;
var callInput;
var callOutput;
var responseDetail;

function drawTrace() {
    drawDecision(trace[0]);
    for (var i = 1; i < trace.length; i++) {
        var path = edges[trace[i-1].d+trace[i].d];
        if (path)
            drawPath(path);
        else
            console.log("no known path from "+trace[i-1].d+" to "+trace[i].d);

        drawDecision(trace[i]);
    }

    var path = edges[trace[i-1].d+response.code];
    if (path) {
        drawPath(path);
        drawResponse(ends[path[path.length-1]]);
    } else {
        console.log("no known path from "+trace[i-1].d+" to "+response.code);
        drawOutOfOrderResponse(trace[i-1]);
    }
};

function moveResponseWindow() {
    responseDetail.style.left = (response.x > 1569
                                 ? response.x-responseDetail.clientWidth
                                 : response.x)+'px';
    responseDetail.style.top = (response.y > 1092
                                ? response.y-responseDetail.clientHeight
                                : response.y)+'px';
};

function drawResponse(end) {
    var context = canvas.getContext('2d');
    context.strokeStyle=HIGHLIGHT;
    context.lineWidth=3;

    response.x = cols[end.col];
    response.y = rows[end.row];
    response.width = end.width;

    context.beginPath();
    context.rect(response.x-(response.width/2),
                 response.y-19,
                 response.width,
                 38);
    context.stroke();

    moveResponseWindow();
};

function drawOutOfOrderResponse(lastdec) {
    var context = canvas.getContext('2d');
    context.strokeStyle=REGULAR;
    context.lineWidth=3;

    response.x = lastdec.x+50;
    response.y = lastdec.y-50;
    response.width = 38;

    context.beginPath();
    context.moveTo(lastdec.x, lastdec.y);
    context.lineTo(response.x, response.y);
    context.stroke();

    context.strokeStyle='#ff0000';
    context.beginPath();
    context.arc(response.x, response.y, 19,
                0, 2*3.14159, false);
    context.stroke();

    moveResponseWindow();
};

function drawDecision(dec) {
    var context = canvas.getContext('2d');

    if (onlyNotExportedCalls(dec))
        context.strokeStyle=REGULAR;
    else
        context.strokeStyle=HIGHLIGHT;
    context.lineWidth=3;

    dec.x = cols[dec.d[0]];
    dec.y = rows[dec.d.slice(1)];

    context.beginPath();
    context.moveTo(dec.x,    dec.y-19);
    context.lineTo(dec.x+19, dec.y);
    context.lineTo(dec.x,    dec.y+19);
    context.lineTo(dec.x-19, dec.y);
    context.closePath();
    context.stroke();
};

function onlyNotExportedCalls(dec) {
    for (var i = dec.calls.length-1; i >= 0; i--) {
        if (dec.calls[i].output != "wmtrace_not_exported")
            return false;
    }
    return true;
};

function drawPath(path) {
    var context = canvas.getContext('2d');
    context.strokeStyle=REGULAR;
    context.lineWidth=3;

    context.beginPath();
    var seg = getSeg(path[0], path[1], path.length==2);
    context.moveTo(seg.x1, seg.y1);
    context.lineTo(seg.x2, seg.y2);
    for (var p = 2; p < path.length; p++) {
        seg = getSeg(path[p-1], path[p], p == path.length-1);
        context.lineTo(seg.x2, seg.y2);
    }
    context.stroke();
};

function getSeg(p1, p2, last) {
    var seg = {
        x1:cols[p1[0]],
        y1:rows[p1.slice(1)]
    };
    if (ends[p2]) {
        seg.x2 = cols[ends[p2].col];
        seg.y2 = rows[ends[p2].row];
    } else {
        seg.x2 = cols[p2[0]];
        seg.y2 = rows[p2.slice(1)];
    }

    if (seg.x1 == seg.x2) {
        if (seg.y1 < seg.y2) {
            seg.y1 = seg.y1+19;
            if (last) seg.y2 = seg.y2-19;
        } else {
            seg.y1 = seg.y1-19;
            if (last) seg.y2 = seg.y2+19;
        }
    } else {
        //assume seg.y1 == seg.y2
        if (seg.x1 < seg.x2) {
            seg.x1 = seg.x1+19;
            if (last) seg.x2 = seg.x2-(ends[p2] ? (ends[p2].width/2) : 19);
        } else {
            seg.x1 = seg.x1-19;
            if (last) seg.x2 = seg.x2+(ends[p2] ? (ends[p2].width/2) : 19);
        }
    }
    return seg;
};

function findDecision(ev) {
    var x = ev.clientX+window.pageXOffset;
    var y = ev.clientY+window.pageYOffset;
        
    for (var i = trace.length-1; i >= 0; i--) {
        if (x >= trace[i].x-19 && x <= trace[i].x+19 &&
            y >= trace[i].y-19 && y <= trace[i].y+19)
            return trace[i];
    }
};

function overResponse(ev) {
    var x = ev.clientX+window.pageXOffset;
    var y = ev.clientY+window.pageYOffset;

    return (x >= response.x-(response.width/2)
            && x <= response.x+(response.width/2)
            && y >= response.y-19 && y <= response.y+19);
};

function traceDecision(name) {
    for (var i = trace.length-1; i >= 0; i--)
        if (trace[i].d == name) return trace[i];
};

function headersList(headers) {
    var h = '';
    for (n in headers) {
        h += '<li>'+n+': '+headers[n];
    }
    return h;
};

function fillRequestDetail() {
    document.getElementById('requestmethod').textContent = request.method;
    document.getElementById('requestpath').textContent = request.path;
    document.getElementById('requestheaders').innerHTML = headersList(request.headers);
    document.getElementById('requestbody').textContent = request.body;
};

function fillResponseDetail() {
    document.getElementById('responsecode').textContent = response.code;
    document.getElementById('responseheaders').innerHTML = headersList(response.headers);
    document.getElementById('responsebody').textContent = response.body;
};

window.onload = function() {
    canvas = document.getElementById('v3map');
    detail = document.getElementById('decisiondetail');
    decisionId = document.getElementById('decisionid');
    decisionCalls = document.getElementById('decisioncalls');
    callInput = document.getElementById('callinput');
    callOutput = document.getElementById('calloutput');
    responseDetail = document.getElementById('responsedetail');

    decisionCalls.onchange = function() {
        var val = decisionCalls.value;
        if (val) {
            var dec = traceDecision(val.substring(0, val.indexOf('-')));
            var call = dec.calls[parseInt(val.substring(val.indexOf('-')+1, val.length))];

            if (call.output != "wmtrace_not_exported") {
                callInput.style.color='#000000';
                callInput.textContent = call.input;
                if (call.output != null) {
                    callOutput.style.color = '#000000';
                    callOutput.textContent = call.output;
                } else {
                    callOutput.style.color = '#ff0000';
                    callOutput.textContent = 'Error: '+call.module+':'+call['function']+' never returned';
                }
            } else {
                callInput.style.color='#999999';
                callInput.textContent = call.module+':'+call['function']+' was not exported';
                callOutput.textContent = '';
            }
        }
    };

    fillRequestDetail();
    fillResponseDetail();

    var bg = new Image(3138, 2184);

    bg.onload = function() {
        canvas.getContext("2d").drawImage(bg, 0, 0);
        drawTrace();

        canvas.onmousemove = function(ev) {
            if (findDecision(ev) || overResponse(ev))
                canvas.style.cursor = 'pointer';
            else
                canvas.style.cursor = 'default';
        };

        canvas.onclick = function(ev) {
            var dec = findDecision(ev);
            if (dec) {
                decisionId.innerHTML = dec.d;

                var calls = [];
                for (var i = 0; i < dec.calls.length; i++) {
                    calls.push('<option value="'+dec.d+'-'+i+'">');
                    calls.push(dec.calls[i].module+':'+dec.calls[i]['function']);
                    calls.push('</option>');
                }
                decisionCalls.innerHTML = calls.join('');
                decisionCalls.selectedIndex = 0;

                decisionCalls.onchange();
                
                if (dec.x > 1569)
                    detail.style.left = (dec.x-detail.clientWidth)+'px';
                else
                    detail.style.left = dec.x+'px';

                if (dec.y > 1092)
                    detail.style.top = (dec.y-detail.clientHeight)+'px';
                else
                    detail.style.top = dec.y+'px';

                detail.style.visibility = 'visible';
                responseDetail.style.visibility = 'hidden';
            } else {
                detail.style.visibility = 'hidden';
                if (overResponse(ev))
                    responseDetail.style.visibility = 'visible';
                else
                    responseDetail.style.visibility = 'hidden';
            }
        };

    };

    bg.onerror = function() {
        alert('Failed to load background image.');
    };

    bg.src = 'static/map.png';
};
