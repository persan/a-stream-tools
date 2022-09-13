
with Ada.Streams;
with System.Unsigned_Types;
package Stream_Tools.Tests.BUG20220913_Low_Defs is

   use System.Unsigned_Types;
   subtype Message_Id_T is Unsigned range 16#0# .. 16#D#;

   type Header_T is
      record
         Msg_Id        : Message_Id_T := Message_Id_T'First;
         Msg_Send_Time : Long_Unsigned := 0;
         Msg_Length    : Unsigned := 0;
      end record;

   type Non_Streamable_Link_Header is new Header_T with
     Read => Read,
     Write => Write;
   procedure Read (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Non_Streamable_Link_Header) is null;
   procedure Write (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : Non_Streamable_Link_Header) is null;

   type Jp_List_Distribution_Acknowledge_T is
      record
         Jp_List_Receiving_Status : Boolean := False;
      end record with
     Read => Read,
     Write => Write;
   procedure Read (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : out Jp_List_Distribution_Acknowledge_T);
   procedure Write (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : Jp_List_Distribution_Acknowledge_T);
end Stream_Tools.Tests.BUG20220913_Low_Defs;
