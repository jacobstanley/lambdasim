@import <Foundation/CPObject.j>

@import "CPView.j"

@implementation AppController : CPObject
{
    CPTextField title;

    CPSlider speed;
    CPSlider heading;
    CPSlider rudder;
}

- (void)applicationDidFinishLaunching:(CPNotification)aNotification
{
    var theWindow = [[CPWindow alloc]
        initWithContentRect:CGRectMakeZero()
        styleMask:CPBorderlessBridgeWindowMask];

    var contentView = [theWindow contentView];

    // title

    title = [[CPTextField alloc]
        initWithFrame:CGRectMakeZero()];

    [title setStringValue:@"Lambdaλsim"];
    [title setFont:[CPFont boldSystemFontOfSize:24.0]];
    
    [title sizeToFit];
    [title keepCentered];
    [title setAlignment:CPCenterTextAlignment];
    [title setCenter:CGPointMake([contentView center].x, 35)];

    [contentView addSubview:title];

    // speed slider

    sliderWidth = 175;
    sliderHeight = 20;
    x0 = (CGRectGetWidth([contentView bounds]) - sliderWidth) / 2;
    y0 = CGRectGetMaxY([title frame]) + 15;

    speed = [[CPSlider alloc]
        initWithFrame:CGRectMake(x0, y0, sliderWidth, sliderHeight)];

    [speed setTarget:self];
    [speed setAction:@selector(speedChanged:)];
    [speed setMinValue:-5];
    [speed setMaxValue:20];
    [speed setDoubleValue:0];
    [speed keepCentered];

    [contentView addSubview:speed];

    // speed slider labels

    var speedLabel = [self labelWithTitle:"Speed (knots)"],
        speedStartLabel = [self labelWithTitle:"-5"],
        speedEndLabel = [self labelWithTitle:"20"];

    [speedLabel setFont:[CPFont systemFontOfSize:13.0]];
    [speedLabel setAlignment:CPCenterTextAlignment];
    [speedLabel setCenter:CGPointMake(
        [speed center].x, CGRectGetMaxY([speed frame]) + 15)];

    [speedStartLabel setFrameOrigin:CGPointMake(
            x0 - CGRectGetWidth([speedStartLabel frame]) - 5, y0)];
    [speedEndLabel setFrameOrigin:CGPointMake(
            CGRectGetMaxX([speed frame]) + 5, y0)];

    [contentView addSubview:speedLabel];
    [contentView addSubview:speedStartLabel];
    [contentView addSubview:speedEndLabel];

    var request = [CPURLRequest requestWithURL:"vessel/speed"];
    [CPURLConnection connectionWithRequest:request delegate:self];

    [theWindow orderFront:self];
}

- (void)speedChanged:(id)sender
{
    var request = [CPURLRequest requestWithURL:
        "vessel/speed/" + [speed doubleValue]]
    [request setHTTPMethod:"PUT"]
    [CPURLConnection connectionWithRequest:request delegate:nil];
}

- (void)connection:(CPURLConnection)connection didReceiveData:(CPString)data
{
    var result = JSON.parse(data)
    [title setStringValue:result.eText]
}

- (void)connection:(CPURLConnection)connection didFailWithError:(CPString)error
{
    [label setStringValue:error]
}

- (CPTextField)labelWithTitle:(CPString)aTitle
{
    var label = [[CPTextField alloc] initWithFrame:CGRectMakeZero()];

    [label setStringValue:aTitle];
    [label setFont:[CPFont systemFontOfSize:16.0]];
    [label sizeToFit];
    [label keepCentered];

    return label;
}

@end
