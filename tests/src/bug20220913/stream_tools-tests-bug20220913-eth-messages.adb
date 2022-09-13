with Ada.Containers.Ordered_Maps;
with Ada.Tags.Generic_Dispatching_Constructor;
with Ada.Unchecked_Conversion;
with Stream_Tools.Tests.BUG20220913_Low_Defs; use Stream_Tools.Tests.BUG20220913_Low_Defs;

package body Stream_Tools.Tests.BUG20220913.Eth.Messages is
   use type Ada.Tags.Tag;
   function "<" (L, R : Ada.Tags.Tag) return Boolean;

   function "<" (L, R : Ada.Tags.Tag) return Boolean is
      function Conv is new Ada.Unchecked_Conversion (Ada.Tags.Tag, System.Address);
      use System;
   begin
      return Conv (L) < Conv (R);
   end "<";

   package Id_To_Tag_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => System.Unsigned_Types.Unsigned,
      Element_Type => Ada.Tags.Tag,
      "="          => Ada.Tags."=");

   package Tag_To_Id_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Ada.Tags.Tag,
      Element_Type => System.Unsigned_Types.Unsigned);

   --  package Tag_To_Source_Maps is new Ada.Containers.Ordered_Maps
   --  (Key_Type     => Ada.Tags.Tag,
   --   Element_Type => System.Unsigned_Types.Short_Short_Unsigned);

   Id_To_Tag_Map : Id_To_Tag_Maps.Map;
   Tag_To_Id_Map : Tag_To_Id_Maps.Map;
   --  Tag_2_Source_Map : Tag_To_Source_Maps.Map;

   --  =====================================================================
   --  Input Function
   --  =====================================================================

   function Input (S : not null access Ada.Streams.Root_Stream_Type'Class) return Root_Message_Type'Class is
      function Dispatching_Constructor is new Ada.Tags.Generic_Dispatching_Constructor
        (T => Root_Message_Type, Parameters => Ada.Streams.Root_Stream_Type'Class, Constructor => Constructor);
      Header    : Header_T;
   begin
      Header_T'Read (S, Header);

      return Ret : Root_Message_Type'Class := Dispatching_Constructor (Id_To_Tag_Map (Header.Msg_Id), S) do
         Ret.Header := Non_Streamable_Link_Header (Header);
      end return;
   end Input;

   --  =====================================================================
   --  Output Procedure
   --  =====================================================================

   procedure Output (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : Root_Message_Type'Class) is
      Header    : Header_T := Header_T (Item.Header);
      Numeric_Time : constant  := 1000;
      use System.Unsigned_Types;
   begin
      Header.Msg_Send_Time := Long_Unsigned (Numeric_Time);

      Header.Msg_Id := Tag_To_Id_Map (Item'Tag);
      Header.Msg_Length := Item.Length + (Header'Size / 8);

      Header_T'Write (S, Header);
      Root_Message_Type'Class'Write (S, Item);

   end Output;

   --  =====================================================================
   --  Register Procedure
   --  =====================================================================
   procedure Register
     (Message_Id : System.Unsigned_Types.Unsigned; Tag : Ada.Tags.Tag)
   is
   begin
      Id_To_Tag_Map.Insert (Message_Id, Tag);
      Tag_To_Id_Map.Insert (Tag, Message_Id);
   end Register;

end Stream_Tools.Tests.BUG20220913.Eth.Messages;
