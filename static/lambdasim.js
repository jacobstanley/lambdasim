layout = uki({
    view: 'Box',
    rect: '200 100',
    anchors: 'left top right bottom',
    childViews: [{
        view: 'Button', rect: '50 10 100 24', anchors: 'top',
        text: 'Lambda\u03bbsim',
    },
    {
        view: 'Label', rect: '0 50 1', anchors: 'top',
        id: 'speed-label',
    },
    {
        view: 'Slider', rect: '0 70 200 0', anchors: 'top',
        id: 'speed', url: '/vessel/speed',
        label: 'speed-label', header: 'Speed', units: ' knots',
        value: 0, min: -5, max: 20,
    },
    {
        view: 'Label', rect: '0 90 1', anchors: 'top',
        id: 'heading-label',
    },
    {
        view: 'Slider', rect: '0 110 200 0', anchors: 'top',
        id: 'heading', url: '/vessel/heading',
        label: 'heading-label', header: 'Heading', units: '\u00b0T',
        value: 0, min: 0, max: 360,
    },
    {
        view: 'Label', rect: '0 130 1', anchors: 'top',
        id: 'rudder-label',
    },
    {
        view: 'Slider', rect: '0 150 200 0', anchors: 'top',
        id: 'rudder', url: '/vessel/rudder',
        label: 'rudder-label', header: 'Rudder', units: '\u00b0/s',
        value: 0, min: -5, max: 5,
    },
    ]
});

var updating = false;

function bindSlider(id) {
    uki(id).change(function() {
        var v = this.value().toFixed(1);
        var l = uki('#' + this.label);
        l.text(this.header + ': ' + v + this.units);

        if (!updating) {
            uki.ajax({ type: 'PUT', url: this.url + '/' + v });
        }
    });
}

bindSlider('#speed');
bindSlider('#heading');
bindSlider('#rudder');

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
    var oldValue = uki(id).value();
    if (oldValue != newValue) {
        uki(id).value(newValue).change();
    }
}

fetchValues();
layout.attachTo(window, '200 0');

