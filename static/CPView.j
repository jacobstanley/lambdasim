@implementation CPView (Common)

- (void)keepCenteredInMiddle
{
    [self setAutoresizingMask:
        CPViewMinXMargin |
        CPViewMaxXMargin |
        CPViewMinYMargin |
        CPViewMaxYMargin];
}

- (void)keepCentered
{
    [self setAutoresizingMask:
        CPViewMinXMargin |
        CPViewMaxXMargin];
}

@end
