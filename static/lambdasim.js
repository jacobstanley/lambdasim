function mkSlider(name, header, min, max, step, units) {
    var group = $('#' + name);
    group.append('<h3 id="' + name + '-label"></h3>');
    group.append('<div id="' + name + '-slider" class="slider"></div>');

    var slider = $('#' + name + '-slider');
    var label = $('#' + name + '-label');

    slider.slider({
        value: 0, min: min, max: max, step: step,
        animate: true, width: 300,
        slide: onChange,
        change: onChange,
    });

    function onChange(ev, ui) {
        updateLabel(ui.value);
    }

    var digits = step < 1 ? 1 : 0;

    function updateLabel(value) {
        label.text(header + ': ' + value.toFixed(digits) + units);
    }

    updateLabel(0);
}
