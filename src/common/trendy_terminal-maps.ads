with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

package Trendy_Terminal.Maps is
    package ASU renames Ada.Strings.Unbounded;

    type Key is (Key_Up, Key_Left, Key_Right, Key_Down,
                  Key_F1, Key_F2, Key_F3, Key_F4,
                  Key_F5, Key_F6, Key_F7, Key_F8,
                  Key_F9, Key_F10, Key_F11, Key_F12,
                  Key_Backspace, Key_Pause, Key_Escape,
                  Key_Home, Key_End,
                  Key_Insert, Key_Delete,
                  Key_Page_Up, Key_Page_Down,
                  Key_Tab,

                  -- Keys with modifiers.
                  Key_Shift_Tab,
                  Key_Ctrl_Up, Key_Ctrl_Left, Key_Ctrl_Right, Key_Ctrl_Down
                  );

    package Key_Maps is new Ada.Containers.Ordered_Maps (Key_Type => ASU.Unbounded_String,
                                                         Element_Type => Key,
                                                         "<" => ASU."<",
                                                         "=" => "=");

    package Inverse_Key_Maps is new Ada.Containers.Ordered_Maps (Key_Type => Key,
                                                         Element_Type => ASU.Unbounded_String,
                                                         "<" => "<",
                                                         "=" => ASU."=");

    function Make_Key_Map return Key_Maps.Map;
    function Make_Key_Lookup_Map return Inverse_Key_Maps.Map;
end Trendy_Terminal.Maps;
