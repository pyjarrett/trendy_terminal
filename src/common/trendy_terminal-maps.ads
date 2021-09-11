with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

package Trendy_Terminal.Maps is
    package ASU renames Ada.Strings.Unbounded;

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
