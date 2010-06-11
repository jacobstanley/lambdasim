@import <Foundation/CPObject.j>

@import "CPView.j"

@implementation AppController : CPObject
{
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

    // Title

    title = [[CPTextField alloc]
        initWithFrame:CGRectMakeZero()];

    [title setStringValue:@"Lambdaλsim"];
    [title setFont:[CPFont boldSystemFontOfSize:24.0]];
    
    [title sizeToFit];
    [title keepCentered];
    [title setAlignment:CPCenterTextAlignment];
    [title setCenter:CGPointMake([contentView center].x, 35)];

    [contentView addSubview:title];

    // Sliders

    sliderWidth = 175;
    sliderHeight = 20;
    x0 = (CGRectGetWidth([contentView bounds]) - sliderWidth) / 2;
    y0 = CGRectGetMaxY([title frame]) + 15;

    speed = [[CPSlider alloc]
        initWithFrame:CGRectMake(x0, y0, sliderWidth, sliderHeight)];

    /*
    [speed setTarget:self];
    [speed setAction:@selector(adjustSpeed:)];*/
    [speed setMinValue:-5];
    [speed setMaxValue:20];
    [speed setDoubleValue:0];
    [speed keepCentered];

    [contentView addSubview:speed];

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

    [theWindow orderFront:self];
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

/*
- (void)swap:(id)sender
{
    if ([title stringValue] == @"Hello Lambdaλsim!")
        [title setStringValue:"Goodbye!"];
    else
        [title setStringValue:@"Hello Lambdaλsim!"];
}
*/

@end
