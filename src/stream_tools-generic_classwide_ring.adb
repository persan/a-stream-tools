with Stream_Tools.Memory_Streams;
with GNAT.Semaphores; use GNAT.Semaphores;
package body Stream_Tools.Generic_Classwide_Ring is

   package Locks is

      subtype Mutual_Exclusion is Binary_Semaphore
        (Initially_Available => True,
         Ceiling             => Default_Ceiling);
      type Scoped_Lock (Lock : access Mutual_Exclusion)
      is new Ada.Finalization.Limited_Controlled with null record;
      overriding procedure Initialize (This : in out Scoped_Lock);
      overriding procedure Finalize   (This : in out Scoped_Lock);
   end Locks;
   package body Locks is

      ----------------
      -- Initialize --
      ----------------

      overriding procedure Initialize (This : in out Scoped_Lock) is
      begin
         This.Lock.Seize;
      end Initialize;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (This : in out Scoped_Lock) is
      begin
         This.Lock.Release;
      end Finalize;
   end Locks;

   use Ada.Streams;
   ------------------
   -- First_Cursor --
   ------------------

   function First_Cursor (Cont : Ring_Buffer) return Cursor is
   begin
      return
        Cursor'(C => Cont'Unrestricted_Access, Controll => (Ada.Finalization.Controlled with C => Cont'Unrestricted_Access));
   end First_Cursor;

   -------------
   -- Advance --
   -------------

   function Advance (Cont : Ring_Buffer; Position : Cursor) return Cursor is
      pragma Unreferenced (Cont);
   begin
      return Position;
   end Advance;

   ------------------------
   -- Cursor_Has_Element --
   ------------------------

   function Cursor_Has_Element (Cont : Ring_Buffer; Position : Cursor) return Boolean is
      pragma Unreferenced (Position);
   begin
      return not Cont.End_Of_File;
   end Cursor_Has_Element;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element (Cont : Ring_Buffer; Position : Cursor) return T'Class is
      pragma Unreferenced (Position);
      S : constant access Ring_Buffer := Cont'Unrestricted_Access;
   begin
      return S.Input;
   end Get_Element;

   ------------
   -- Output --
   ------------

   procedure Output (S : access Ring_Buffer; Item : T'Class) is
      Lock          : Locks.Scoped_Lock (S.Lock'Access) with
        Unreferenced;
      Output_Buffer : aliased Stream_Tools.Memory_Streams.Memory_Stream;
   begin
      --  This construct is to force thw whole object to be "written" in one operation.
      --
      --  Serialize the object
      Output_Buffer.Set_Address (S.Temp'Address);
      Output_Buffer.Set_Length (S.Temp'Length);
      Output_Buffer.Reset;
      T'Class'Output (Output_Buffer'Access, Item);
      --  Store the serialized data.
      Memory_Streams.Memory_Stream'Write (S, Output_Buffer);
   end Output;

   -----------
   -- Input --
   -----------

   function Input (S : access Ring_Buffer) return T'Class is
      Lock : Locks.Scoped_Lock (S.Lock'Access) with
        Unreferenced;
   begin
      return T'Class'Input (S);
   end Input;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (S : access constant Ring_Buffer) return Boolean is
   begin
      return S.Head = S.Tail;
   end End_Of_File;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Ring_Buffer; Item : out Ada.Streams.Stream_Element_Array; Last : out Ada.Streams.Stream_Element_Offset)
   is
   --       L_Last : Ada.Streams.Stream_Element_Offset := Stream.Tail + Item'Length;
   begin
      for I of Item loop
         I           := Stream.Buffer (Stream.Tail);
         Stream.Tail := (Stream.Tail + 1) mod (Stream.Capacity + 1);
      end loop;
      Last := Item'Last;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write (Stream : in out Ring_Buffer; Item : Ada.Streams.Stream_Element_Array) is
   begin
      for I of Item loop
         Stream.Buffer (Stream.Head) := I;
         Stream.Head                 := (Stream.Head + 1) mod (Stream.Capacity + 1);
      end loop;
   end Write;

   procedure Reset (S : access Ring_Buffer; Hard : Boolean := False) is
   begin
      S.Head := 0;
      S.Tail := 0;
      if Hard then
         S.Buffer := [others => 0];
      end if;
   end Reset;

end Stream_Tools.Generic_Classwide_Ring;
