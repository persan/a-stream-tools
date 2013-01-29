with Ada.Directories;

package body Stream_Tools.Tests is

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Object : in out Binary_Semaphore_Key) is
   begin
      Object.Lock.Seize;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Binary_Semaphore_Key) is
   begin
      Object.Lock.Release;
   end Finalize;
   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Object : in out Counting_Semaphore_Key) is
   begin
      Object.Lock.Seize;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Counting_Semaphore_Key) is
   begin
      Object.Lock.Release;
   end Finalize;

   overriding procedure Initialize (Object : in out Directory_Push) is
   begin
      Object.Data := new String'(Ada.Directories.Current_Directory);
   end Initialize;
   overriding procedure Finalize   (Object : in out Directory_Push) is
   begin
      Ada.Directories.Set_Directory (Object.Data.all);
      GNAT.Strings.Free (Object.Data);
   end Finalize;

end Stream_Tools.Tests;
