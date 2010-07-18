function mkSlider(name, header, units, min, max, step) {
    var group = $('#' + name);
    group.append('<h3 id="' + name + '-label"></h3>')
    group.append('<div id="' + name + '-slider" class="slider"></div>');

    var control = $('#' + name + '-slider');
    var label = $('#' + name + '-label');

    control.slider({
        min: min, max: max, step: step, animate: true,
        change: function(event, ui) {
            labelValue(ui.value);
        },
        slide: function(event, ui) {
            labelValue(ui.value);
            $.put('/vessel/' + name + '/' + ui.value);
            fetcher.ignoreLast();
        },
    }).addTouch();

    var labelDigits = step < 1 ? 1 : 0;
    function labelValue(value) {
        label.text(header + ': ' + value.toFixed(labelDigits) + units);
    }

    control.slider('update', 0);
}

var fetcher = {
    ignoreLast: function() {
        this._min = this._next;
    },
    start: function() {
        this._fetch();
    },
    _next: 0,
    _min: 0,
    _increment: function() {
        var id = this._next;
        this._next++;
        return id;
    },
    _fetch: function() {
        var id = this._increment();
        var that = this;
        $.get('/vessel', function(v) {
            if (id >= that._min) {
                $('#speed-slider').slider('update', v.speed);
                $('#heading-slider').slider('update', v.heading);
                $('#rudder-slider').slider('update', v.rudder);
            }
            setTimeout(function() { that._fetch() }, 500);
        });
    }
};

fetcher.start();


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
    update: function(value) {
        if (this._mouseSliding || this._keySliding) return;
        this.value(value);
    },
});
