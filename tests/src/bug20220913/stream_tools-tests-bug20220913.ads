with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Stream_Tools.Memory_Streams;

package Stream_Tools.Tests.BUG20220913 is
--  gnatpro-19.2 fail to bind due to circular elab.
--  gnatpro-20.2 Crash
--  gnatpro-21.1 Crash.
--  gnatpro-23.0w-20220621 Crash.

   type Test_Case is new Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run

   overriding function Name (T : Test_Case) return Message_String;
   --  Provide name identifying the test case

   type PI_Memory_Stream is new Stream_Tools.Memory_Streams.Dynamic_Memory_Stream (1024, Stream_Tools.Memory_Streams.Multiply_By_Two) with null record;

end Stream_Tools.Tests.BUG20220913;
