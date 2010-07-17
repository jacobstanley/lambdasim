function mkSlider(name, header, units, min, max, step) {
    var group = $('#' + name);
    group.append('<h3 id="' + name + '-label"></h3>')
    group.append('<div id="' + name + '-slider" class="slider"></div>');

    var control = $('#' + name + '-slider');
    var label = $('#' + name + '-label');

    var labelDigits = step < 1 ? 1 : 0;
    var putUrl = '/vessel/' + name + '/';

    control.slider({
        min: min, max: max, step: step,
        animate: true, width: 300,
        slide: onChange,
        change: onChange,
    });

    function onChange(ev, ui) {
        label.text(header + ': ' + ui.value.toFixed(labelDigits) + units);

        if (!control.slider('updating')) {
            $.put(putUrl + ui.value);
        }
    }

    control.slider('update', 0);
}

function fetchValues() {
    $.get('/vessel', function(v) {
        $('#speed-slider').slider('update', v.simSpeed);
        $('#heading-slider').slider('update', v.simHeading);
        $('#rudder-slider').slider('update', v.simRudder);
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

jQuery.fn.extend(jQuery.ui.slider.prototype, {
    _updating: false,
    updating: function() {
        return this._updating;
    },
    update: function(value) {
        if (this._mouseSliding || this._keySliding) return;
        this._updating = true;
        this.value(value);
        this._updating = false;
    },
});
