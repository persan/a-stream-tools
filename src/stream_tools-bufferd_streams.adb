package body Stream_Tools.Bufferd_Streams is

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Bufferd_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
   begin
      Stream.Buffer.Write (Item);
   end Write;
   overriding procedure Read
     (Stream : in out Bufferd_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      Stream.Buffer.Read (Item, Last);
   end Read;

   -----------------
   -- Buffer_Type --
   -----------------

   use Ada.Streams;
   protected body Buffer_Type is
   ----------
   -- Read --
   ----------
      entry Read
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset)
        when Count > 0
      is
      begin
         if Item'Length < Count then
            requeue I_Read (Item'Length);
         else
            H_Read (Item, Last);
         end if;
      end Read;

      -----------
      -- Write --
      -----------

      entry Write
        (Item   : Ada.Streams.Stream_Element_Array)
        when Count < Size
      is
      begin
         if Item'Length > Buffer'Length - Count then
            requeue I_Write (Item'Length);
         else
            H_Write (Item);
         end if;
      end Write;

      entry I_Read (for R_Count in My_Stream_Element_Count)
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset)
      when
        R_Count <= Count  is
      begin
         H_Read (Item, Last);
      end I_Read;

      entry I_Write (for W_Count in My_Stream_Element_Count)
        (Item   : Ada.Streams.Stream_Element_Array)
      when
        W_Count >= Size - Count is
      begin
         H_Write (Item);
      end I_Write;

      procedure H_Read
        (Item   : out Ada.Streams.Stream_Element_Array;
         Last   : out Ada.Streams.Stream_Element_Offset) is
      begin
         if Read_Cursor + Item'Length <= Size then
            Item := Buffer (Read_Cursor .. Read_Cursor + Item'Length - 1);
         else
            declare
               Split : constant Ada.Streams.Stream_Element_Offset :=
                         Item'First + Item'Length - (Buffer'Last - Read_Cursor);
            begin
               Buffer (Read_Cursor .. Buffer'Last) := Item (Item'First .. Split);
               Buffer (Buffer'First .. Item'Last - Split) :=
                 Item (Split + 1 .. Item'Last);
            end;
         end if;
         Last := Item'Last;
      end H_Read;

      procedure H_Write
        (Item   : Ada.Streams.Stream_Element_Array) is
      begin
         if Write_Cursor + Item'Length <= Size then
            Buffer (Write_Cursor .. Write_Cursor + Item'Length - 1) := Item;
         else
            declare
               Split : constant Ada.Streams.Stream_Element_Offset :=
                         Item'First + Item'Length - (Buffer'Last - Write_Cursor);
            begin
               Buffer (Write_Cursor .. Buffer'Last) := Item (Item'First .. Split);
               Buffer (Buffer'First .. Item'Last - Split) :=
                 Item (Split + 1 .. Item'Last);
            end;
         end if;
      end H_Write;

   end Buffer_Type;

end Stream_Tools.Bufferd_Streams;
