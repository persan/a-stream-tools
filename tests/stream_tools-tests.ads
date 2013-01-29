with GNAT.Semaphores;
with Ada.Finalization;
with GNAT.Strings;
package Stream_Tools.Tests is
   type Annotations is (Top_Level, Key);
   type Foo is record
      D  : String (1 .. 10); pragma Annotate (D, Key);
      C  : String (1 .. 10);
   end record; pragma Annotate (Foo, Top_Level);

   type Binary_Semaphore_Key
     (Lock : not null access GNAT.Semaphores.Binary_Semaphore)
     is new Ada.Finalization.Limited_Controlled with null record with
   Unreferenced_Objects;

   overriding procedure Initialize (Object : in out Binary_Semaphore_Key);
   overriding procedure Finalize   (Object : in out Binary_Semaphore_Key);

   type Counting_Semaphore_Key
     (Lock : not null access GNAT.Semaphores.Counting_Semaphore)
     is new Ada.Finalization.Limited_Controlled with null record with
   Unreferenced_Objects;

   overriding procedure Initialize (Object : in out Counting_Semaphore_Key);
   overriding procedure Finalize   (Object : in out Counting_Semaphore_Key);

   type Directory_Push
     is new Ada.Finalization.Limited_Controlled with record
      Data : GNAT.Strings.String_Access;
   end record with
   Unreferenced_Objects;

   overriding procedure Initialize (Object : in out Directory_Push);
   overriding procedure Finalize   (Object : in out Directory_Push);

end Stream_Tools.Tests;
