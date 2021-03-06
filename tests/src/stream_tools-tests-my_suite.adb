with Stream_Tools.Tests.Memory_Stream_Tests;
with Stream_Tools.Tests.Bufferd_Stream_Tests;
package body Stream_Tools.Tests.My_Suite is

   use AUnit.Test_Suites;

   --  Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_1 : aliased  Stream_Tools.Tests.Bufferd_Stream_Tests.Bufferd_Stream_Test;
   Test_2 : aliased  Stream_Tools.Tests.Memory_Stream_Tests.Memory_Stream_Test;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      Add_Test (Result'Access, Test_2'Access);
      return Result'Access;
   end Suite;

end Stream_Tools.Tests.My_Suite;
