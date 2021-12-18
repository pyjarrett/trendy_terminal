with Trendy_Test.Reports;
with Trendy_Terminal.Histories.Tests;

procedure Trendy_Terminal_Tests is
begin
    Trendy_Test.Register (Trendy_Terminal.Histories.Tests.All_Tests);

    Trendy_Test.Reports.Print_Basic_Report (Trendy_Test.Run);
end Trendy_Terminal_Tests;
