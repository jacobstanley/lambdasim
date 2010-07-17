layout = uki({
    view: 'Box', rect: '200 100', anchors: 'left top right bottom',
    id: 'box',
    childViews: [
    {
        view: 'Label', rect: '0 0 1', anchors: 'top',
        id: 'speed-label',
    },
    {
        view: 'Slider', rect: '0 20 200 0', anchors: 'top',
        id: 'speed', url: '/vessel/speed',
        label: 'speed-label', header: 'Speed', units: ' knots',
        value: 0, values: steps(-5, 20),
    },
    {
        view: 'Label', rect: '0 50 1', anchors: 'top',
        id: 'heading-label',
    },
    {
        view: 'Slider', rect: '0 70 200 0', anchors: 'top',
        id: 'heading', url: '/vessel/heading',
        label: 'heading-label', header: 'Heading', units: '\u00b0T',
        value: 0, values: steps(0, 360),
    },
    {
        view: 'Label', rect: '0 100 1', anchors: 'top',
        id: 'rudder-label',
    },
    {
        view: 'Slider', rect: '0 120 200 0', anchors: 'top',
        id: 'rudder', url: '/vessel/rudder',
        label: 'rudder-label', header: 'Rudder', units: '\u00b0/s',
        value: 0, values: steps(-5, 5),
    },
    ]
});

bindSlider('#speed');
bindSlider('#heading');
bindSlider('#rudder');

fetchValues();
layout.attachTo(document.getElementById('content'), '200 0');

var updating = false;

function steps(min, max) {
    var values = [];
    for (i = min*10; i <= max*10; i++) {
        values.push(i/10);
    }
    return values;
}

function bindSlider(id) {
    uki(id).bind('change', function() {
        var v = this.value();
        uki('#' + this.label).text(this.header + ': ' + v + this.units);

        if (!updating) {
            uki.ajax({ type: 'PUT', url: this.url + '/' + v });
        }
    });
}

function fetchValues() {
    uki.getJSON('/vessel', function(v) {
        updating = true;
        updateValue('#speed', v.simSpeed);
        updateValue('#heading', v.simHeading);
        updateValue('#rudder', v.simRudder);
        updating = false;
        setTimeout(fetchValues, 500);
    });
}

function updateValue(id, newValue) {
    uki(id).each(function() {
        if (this._dragging) { return; }
        if (newValue == this.value()) return;
        this.value(newValue).trigger('change');
    });
}

