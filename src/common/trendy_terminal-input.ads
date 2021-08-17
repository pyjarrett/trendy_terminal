package Trendy_Terminal.Input is

    type Key_Identifier is
       (Key_Left, Key_Right, Key_Up, Key_Down, Key_Insert, Key_Delete,
        Key_Home, Key_End, Key_Page_Down, Key_Page_Up, Key_F1, Key_F2, Key_F3,
        Key_F4, Key_F5, Key_F6, Key_F7, Key_F8, Key_F9, Key_F10, Key_F11,
        Key_F12);

    type Modifier is
       (Modifier_Shift, Modifier_Ctrl, Modifier_Meta, Modifier_None);

    -- A description of a keypress.
    --
    -- A keymap will associate these keypresses against proceduces which can
    -- be called.
    type Key_Press is private;

    -- A character pressed, along with control.
    -- function Make_Key_Press (C : Character) return Key_Press;

    -- A list of keybindings
    -- function Add_Key_Bind(K : Key_Press; Fn : access all procedure);

    function Get_Line return String;

private

    type Key_Press is record
        -- A modifier which might be pressed.
        M    : Modifier;

        -- Numeric identifier of a given key.  This might not be a specific
        -- letter, it might be something like a "Up Arrow" or "Page Down".
        Code : Integer;
    end record;

end Trendy_Terminal.Input;
