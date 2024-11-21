with Window; use Window;

package renderer is

    procedure set_pixel_color (img : in out image; 
                                x : natural; 
                                y : natural;
                                c : color);

    procedure line (x0 : in out natural;
                    y0 : in out natural;
                    x1 : in out natural; 
                    y1 : in out natural; 
                    c : color; 
                    img : in out image);

    procedure Draw_Regular_Polygon (img : in out Image;
        Sides : Positive;
        Radius : Positive;
        Center_X : Float;
        Center_Y : Float;
        c : Color);

    generic
        type t is private;
    procedure generic_swap (x, y : in out t);

end renderer;