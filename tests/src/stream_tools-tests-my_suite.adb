with Stream_Tools.Tests.Memory_Stream_Tests;
with Stream_Tools.Tests.Bufferd_Stream_Tests;
with Stream_Tools.Tests.Generic_Classwide_Ring;
with Stream_Tools.Tests.BUG20220913.Test;
package body Stream_Tools.Tests.My_Suite is

   use AUnit.Test_Suites;

   --  Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_1 : aliased Stream_Tools.Tests.Bufferd_Stream_Tests.Bufferd_Stream_Test;
   Test_2 : aliased Stream_Tools.Tests.Memory_Stream_Tests.Memory_Stream_Test;
   Test_3 : aliased Stream_Tools.Tests.Generic_Classwide_Ring.Test_Case;
   Test_4 : aliased Stream_Tools.Tests.BUG20220913.Test.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      Add_Test (Result'Access, Test_2'Access);
      Add_Test (Result'Access, Test_3'Access);
      Add_Test (Result'Access, Test_4'Access);
      return Result'Access;
   end Suite;

end Stream_Tools.Tests.My_Suite;
