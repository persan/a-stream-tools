with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Stream_Tools.Tests.Bufferd_Stream_Tests is
   type Bufferd_Stream_Test is new Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Bufferd_Stream_Test);
   --  Register routines to be run

   overriding function Name (T : Bufferd_Stream_Test) return Message_String;
   --  Provide name identifying the test case

   --  Test Routines:
   procedure Test_Simple (T : in out Test_Cases.Test_Case'Class);
end Stream_Tools.Tests.Bufferd_Stream_Tests;
