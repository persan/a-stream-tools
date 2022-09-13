with AUnit.Test_Suites;
with AUnit.Reporter.Text;
with AUnit.Run;
procedure Stream_Tools.Tests.BUG20220913.Test.main is
   use AUnit.Test_Suites;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : Access_Test_Suite;
      pragma Warnings (Off);
   begin
      Result := new Test_Suite;
      Add_Test (Result, new Test_Case);
      return Result;
   end Suite;

   procedure Run is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Stream_Tools.Tests.BUG20220913.Main.Test;
