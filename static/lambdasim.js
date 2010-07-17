function mkSlider(name, header, units, min, max, step) {
    var group = $('#' + name);
    group.append('<h3 id="' + name + '-label"></h3>')
    group.append('<div id="' + name + '-slider" class="slider"></div>');

    var control = $('#' + name + '-slider');
    var label = $('#' + name + '-label');

    control.slider({
        min: min, max: max, step: step,
        animate: true, width: 300,
        change: function(ev, ui) {
            labelValue(ui.value);
        },
        slide: function(ev, ui) {
            labelValue(ui.value);
            $.put('/vessel/' + name + '/' + ui.value);
        },
    });

    var labelDigits = step < 1 ? 1 : 0;
    function labelValue(value) {
        label.text(header + ': ' + value.toFixed(labelDigits) + units);
    }

    control.slider('value', 0);
}

function fetchValues() {
    $.get('/vessel', function(v) {
        $('#speed-slider').slider('value', v.simSpeed);
        $('#heading-slider').slider('value', v.simHeading);
        $('#rudder-slider').slider('value', v.simRudder);
        setTimeout(fetchValues, 500);
    });
}

fetchValues();


//
// jQuery Extensions
//

function _ajax_request(url, data, callback, type, method) {
    if (jQuery.isFunction(data)) {
        callback = data;
        data = {};
    }
    return jQuery.ajax({
        type: method,
        url: url,
        data: data,
        success: callback,
        dataType: type
        });
}

jQuery.extend({
    put: function(url, data, callback, type) {
        return _ajax_request(url, data, callback, type, 'PUT');
    },
    delete_: function(url, data, callback, type) {
        return _ajax_request(url, data, callback, type, 'DELETE');
    }
});
