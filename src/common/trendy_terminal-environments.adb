with Trendy_Terminal.Platform;

package body Trendy_Terminal.Environments is

    function Is_Available(Self : Environment) return Boolean is (Self.Initialized);

    overriding
    procedure Initialize (Self : in out Environment) is
    begin
        Self.Initialized := Trendy_Terminal.Platform.Init;
    end Initialize;

    overriding
    procedure Finalize(Self : in out Environment) is
    begin
        if Self.Initialized then
            Trendy_Terminal.Platform.Shutdown;
        end if;
    end Finalize;

end Trendy_Terminal.Environments;
