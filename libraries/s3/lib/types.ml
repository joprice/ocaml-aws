open Aws.BaseTypes
type calendar = CalendarLib.Calendar.t
module GetBucketAclRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module MetadataEntry =
  struct
    type t = {
      name: String.t option ;
      value: String.t option }
    let make ?name  ?value  () = { name; value }
    let parse xml =
      Some
        {
          name =
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse);
          value =
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.value
              (fun f -> Aws.Query.Pair ("Value", (String.to_query f)));
           Aws.Util.option_map v.name
             (fun f -> Aws.Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.value
              (fun f -> ("Value", (String.to_json f)));
           Aws.Util.option_map v.name (fun f -> ("Name", (String.to_json f)))])
    let of_json j =
      {
        name =
          (Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json);
        value =
          (Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json)
      }
  end
module UserMetadata =
  struct
    type t = MetadataEntry.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map MetadataEntry.parse (Aws.Xml.members "MetadataEntry" xml))
    let to_query v = Aws.Query.to_query_list MetadataEntry.to_query v
    let to_json v = `List (List.map MetadataEntry.to_json v)
    let of_json j = Aws.Json.to_list MetadataEntry.of_json j
  end
module Tag =
  struct
    type t = {
      key: String.t ;
      value: String.t }
    let make ~key  ~value  () = { key; value }
    let parse xml =
      Some
        {
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          value =
            (Aws.Xml.required "Value"
               (Aws.Util.option_bind (Aws.Xml.member "Value" xml)
                  String.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Value", (String.to_query v.value)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Value", (String.to_json v.value));
           Some ("Key", (String.to_json v.key))])
    let of_json j =
      {
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        value =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Value")))
      }
  end
module TagSet =
  struct
    type t = Tag.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all (List.map Tag.parse (Aws.Xml.members "Tag" xml))
    let to_query v = Aws.Query.to_query_list Tag.to_query v
    let to_json v = `List (List.map Tag.to_json v)
    let of_json j = Aws.Json.to_list Tag.of_json j
  end
module Tagging =
  struct
    type t = {
      tag_set: TagSet.t }
    let make ~tag_set  () = { tag_set }
    let parse xml =
      Some
        {
          tag_set =
            (Aws.Xml.required "TagSet"
               (Aws.Util.option_bind (Aws.Xml.member "TagSet" xml)
                  TagSet.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair ("TagSet.member", (TagSet.to_query v.tag_set)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("TagSet", (TagSet.to_json v.tag_set))])
    let of_json j =
      {
        tag_set =
          (TagSet.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSet")))
      }
  end
module StorageClass =
  struct
    type t =
      | STANDARD 
      | REDUCED_REDUNDANCY 
      | STANDARD_IA 
      | ONEZONE_IA 
      | INTELLIGENT_TIERING 
      | GLACIER 
      | DEEP_ARCHIVE 
      | OUTPOSTS 
      | GLACIER_IR 
      | SNOW 
    let str_to_t =
      [("SNOW", SNOW);
      ("GLACIER_IR", GLACIER_IR);
      ("OUTPOSTS", OUTPOSTS);
      ("DEEP_ARCHIVE", DEEP_ARCHIVE);
      ("GLACIER", GLACIER);
      ("INTELLIGENT_TIERING", INTELLIGENT_TIERING);
      ("ONEZONE_IA", ONEZONE_IA);
      ("STANDARD_IA", STANDARD_IA);
      ("REDUCED_REDUNDANCY", REDUCED_REDUNDANCY);
      ("STANDARD", STANDARD)]
    let t_to_str =
      [(SNOW, "SNOW");
      (GLACIER_IR, "GLACIER_IR");
      (OUTPOSTS, "OUTPOSTS");
      (DEEP_ARCHIVE, "DEEP_ARCHIVE");
      (GLACIER, "GLACIER");
      (INTELLIGENT_TIERING, "INTELLIGENT_TIERING");
      (ONEZONE_IA, "ONEZONE_IA");
      (STANDARD_IA, "STANDARD_IA");
      (REDUCED_REDUNDANCY, "REDUCED_REDUNDANCY");
      (STANDARD, "STANDARD")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ObjectCannedACL =
  struct
    type t =
      | Private 
      | Public_read 
      | Public_read_write 
      | Authenticated_read 
      | Aws_exec_read 
      | Bucket_owner_read 
      | Bucket_owner_full_control 
    let str_to_t =
      [("bucket-owner-full-control", Bucket_owner_full_control);
      ("bucket-owner-read", Bucket_owner_read);
      ("aws-exec-read", Aws_exec_read);
      ("authenticated-read", Authenticated_read);
      ("public-read-write", Public_read_write);
      ("public-read", Public_read);
      ("private", Private)]
    let t_to_str =
      [(Bucket_owner_full_control, "bucket-owner-full-control");
      (Bucket_owner_read, "bucket-owner-read");
      (Aws_exec_read, "aws-exec-read");
      (Authenticated_read, "authenticated-read");
      (Public_read_write, "public-read-write");
      (Public_read, "public-read");
      (Private, "private")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module Permission =
  struct
    type t =
      | FULL_CONTROL 
      | WRITE 
      | WRITE_ACP 
      | READ 
      | READ_ACP 
    let str_to_t =
      [("READ_ACP", READ_ACP);
      ("READ", READ);
      ("WRITE_ACP", WRITE_ACP);
      ("WRITE", WRITE);
      ("FULL_CONTROL", FULL_CONTROL)]
    let t_to_str =
      [(READ_ACP, "READ_ACP");
      (READ, "READ");
      (WRITE_ACP, "WRITE_ACP");
      (WRITE, "WRITE");
      (FULL_CONTROL, "FULL_CONTROL")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module Type =
  struct
    type t =
      | CanonicalUser 
      | AmazonCustomerByEmail 
      | Group 
    let str_to_t =
      [("Group", Group);
      ("AmazonCustomerByEmail", AmazonCustomerByEmail);
      ("CanonicalUser", CanonicalUser)]
    let t_to_str =
      [(Group, "Group");
      (AmazonCustomerByEmail, "AmazonCustomerByEmail");
      (CanonicalUser, "CanonicalUser")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module Grantee =
  struct
    type t =
      {
      display_name: String.t option ;
      email_address: String.t option ;
      i_d: String.t option ;
      type_: Type.t ;
      u_r_i: String.t option }
    let make ?display_name  ?email_address  ?i_d  ~type_  ?u_r_i  () =
      { display_name; email_address; i_d; type_; u_r_i }
    let parse xml =
      Some
        {
          display_name =
            (Aws.Util.option_bind (Aws.Xml.member "DisplayName" xml)
               String.parse);
          email_address =
            (Aws.Util.option_bind (Aws.Xml.member "EmailAddress" xml)
               String.parse);
          i_d = (Aws.Util.option_bind (Aws.Xml.member "ID" xml) String.parse);
          type_ =
            (Aws.Xml.required "xsi:type"
               (Aws.Util.option_bind (Aws.Xml.member "xsi:type" xml)
                  Type.parse));
          u_r_i =
            (Aws.Util.option_bind (Aws.Xml.member "URI" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.u_r_i
              (fun f -> Aws.Query.Pair ("URI", (String.to_query f)));
           Some (Aws.Query.Pair ("xsi:type", (Type.to_query v.type_)));
           Aws.Util.option_map v.i_d
             (fun f -> Aws.Query.Pair ("ID", (String.to_query f)));
           Aws.Util.option_map v.email_address
             (fun f -> Aws.Query.Pair ("EmailAddress", (String.to_query f)));
           Aws.Util.option_map v.display_name
             (fun f -> Aws.Query.Pair ("DisplayName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.u_r_i
              (fun f -> ("URI", (String.to_json f)));
           Some ("xsi:type", (Type.to_json v.type_));
           Aws.Util.option_map v.i_d (fun f -> ("ID", (String.to_json f)));
           Aws.Util.option_map v.email_address
             (fun f -> ("EmailAddress", (String.to_json f)));
           Aws.Util.option_map v.display_name
             (fun f -> ("DisplayName", (String.to_json f)))])
    let of_json j =
      {
        display_name =
          (Aws.Util.option_map (Aws.Json.lookup j "DisplayName")
             String.of_json);
        email_address =
          (Aws.Util.option_map (Aws.Json.lookup j "EmailAddress")
             String.of_json);
        i_d = (Aws.Util.option_map (Aws.Json.lookup j "ID") String.of_json);
        type_ =
          (Type.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "xsi:type")));
        u_r_i =
          (Aws.Util.option_map (Aws.Json.lookup j "URI") String.of_json)
      }
  end
module Grant =
  struct
    type t = {
      grantee: Grantee.t option ;
      permission: Permission.t option }
    let make ?grantee  ?permission  () = { grantee; permission }
    let parse xml =
      Some
        {
          grantee =
            (Aws.Util.option_bind (Aws.Xml.member "Grantee" xml)
               Grantee.parse);
          permission =
            (Aws.Util.option_bind (Aws.Xml.member "Permission" xml)
               Permission.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.permission
              (fun f ->
                 Aws.Query.Pair ("Permission", (Permission.to_query f)));
           Aws.Util.option_map v.grantee
             (fun f -> Aws.Query.Pair ("Grantee", (Grantee.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.permission
              (fun f -> ("Permission", (Permission.to_json f)));
           Aws.Util.option_map v.grantee
             (fun f -> ("Grantee", (Grantee.to_json f)))])
    let of_json j =
      {
        grantee =
          (Aws.Util.option_map (Aws.Json.lookup j "Grantee") Grantee.of_json);
        permission =
          (Aws.Util.option_map (Aws.Json.lookup j "Permission")
             Permission.of_json)
      }
  end
module Grants =
  struct
    type t = Grant.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map Grant.parse (Aws.Xml.members "Grant" xml))
    let to_query v = Aws.Query.to_query_list Grant.to_query v
    let to_json v = `List (List.map Grant.to_json v)
    let of_json j = Aws.Json.to_list Grant.of_json j
  end
module ServerSideEncryption =
  struct
    type t =
      | AES256 
      | Aws_kms 
      | Aws_kms_dsse 
    let str_to_t =
      [("aws:kms:dsse", Aws_kms_dsse);
      ("aws:kms", Aws_kms);
      ("AES256", AES256)]
    let t_to_str =
      [(Aws_kms_dsse, "aws:kms:dsse");
      (Aws_kms, "aws:kms");
      (AES256, "AES256")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module Encryption =
  struct
    type t =
      {
      encryption_type: ServerSideEncryption.t ;
      k_m_s_key_id: String.t option ;
      k_m_s_context: String.t option }
    let make ~encryption_type  ?k_m_s_key_id  ?k_m_s_context  () =
      { encryption_type; k_m_s_key_id; k_m_s_context }
    let parse xml =
      Some
        {
          encryption_type =
            (Aws.Xml.required "EncryptionType"
               (Aws.Util.option_bind (Aws.Xml.member "EncryptionType" xml)
                  ServerSideEncryption.parse));
          k_m_s_key_id =
            (Aws.Util.option_bind (Aws.Xml.member "KMSKeyId" xml)
               String.parse);
          k_m_s_context =
            (Aws.Util.option_bind (Aws.Xml.member "KMSContext" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.k_m_s_context
              (fun f -> Aws.Query.Pair ("KMSContext", (String.to_query f)));
           Aws.Util.option_map v.k_m_s_key_id
             (fun f -> Aws.Query.Pair ("KMSKeyId", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("EncryptionType",
                  (ServerSideEncryption.to_query v.encryption_type)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.k_m_s_context
              (fun f -> ("KMSContext", (String.to_json f)));
           Aws.Util.option_map v.k_m_s_key_id
             (fun f -> ("KMSKeyId", (String.to_json f)));
           Some
             ("EncryptionType",
               (ServerSideEncryption.to_json v.encryption_type))])
    let of_json j =
      {
        encryption_type =
          (ServerSideEncryption.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "EncryptionType")));
        k_m_s_key_id =
          (Aws.Util.option_map (Aws.Json.lookup j "KMSKeyId") String.of_json);
        k_m_s_context =
          (Aws.Util.option_map (Aws.Json.lookup j "KMSContext")
             String.of_json)
      }
  end
module S3Location =
  struct
    type t =
      {
      bucket_name: String.t ;
      prefix: String.t ;
      encryption: Encryption.t option ;
      canned_a_c_l: ObjectCannedACL.t option ;
      access_control_list: Grants.t ;
      tagging: Tagging.t option ;
      user_metadata: UserMetadata.t ;
      storage_class: StorageClass.t option }
    let make ~bucket_name  ~prefix  ?encryption  ?canned_a_c_l 
      ?(access_control_list= [])  ?tagging  ?(user_metadata= []) 
      ?storage_class  () =
      {
        bucket_name;
        prefix;
        encryption;
        canned_a_c_l;
        access_control_list;
        tagging;
        user_metadata;
        storage_class
      }
    let parse xml =
      Some
        {
          bucket_name =
            (Aws.Xml.required "BucketName"
               (Aws.Util.option_bind (Aws.Xml.member "BucketName" xml)
                  String.parse));
          prefix =
            (Aws.Xml.required "Prefix"
               (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml)
                  String.parse));
          encryption =
            (Aws.Util.option_bind (Aws.Xml.member "Encryption" xml)
               Encryption.parse);
          canned_a_c_l =
            (Aws.Util.option_bind (Aws.Xml.member "CannedACL" xml)
               ObjectCannedACL.parse);
          access_control_list =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "AccessControlList" xml)
                  Grants.parse));
          tagging =
            (Aws.Util.option_bind (Aws.Xml.member "Tagging" xml)
               Tagging.parse);
          user_metadata =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "UserMetadata" xml)
                  UserMetadata.parse));
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "StorageClass" xml)
               StorageClass.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.storage_class
              (fun f ->
                 Aws.Query.Pair ("StorageClass", (StorageClass.to_query f)));
           Some
             (Aws.Query.Pair
                ("UserMetadata.member",
                  (UserMetadata.to_query v.user_metadata)));
           Aws.Util.option_map v.tagging
             (fun f -> Aws.Query.Pair ("Tagging", (Tagging.to_query f)));
           Some
             (Aws.Query.Pair
                ("AccessControlList.member",
                  (Grants.to_query v.access_control_list)));
           Aws.Util.option_map v.canned_a_c_l
             (fun f ->
                Aws.Query.Pair ("CannedACL", (ObjectCannedACL.to_query f)));
           Aws.Util.option_map v.encryption
             (fun f -> Aws.Query.Pair ("Encryption", (Encryption.to_query f)));
           Some (Aws.Query.Pair ("Prefix", (String.to_query v.prefix)));
           Some
             (Aws.Query.Pair ("BucketName", (String.to_query v.bucket_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.storage_class
              (fun f -> ("StorageClass", (StorageClass.to_json f)));
           Some ("UserMetadata", (UserMetadata.to_json v.user_metadata));
           Aws.Util.option_map v.tagging
             (fun f -> ("Tagging", (Tagging.to_json f)));
           Some ("AccessControlList", (Grants.to_json v.access_control_list));
           Aws.Util.option_map v.canned_a_c_l
             (fun f -> ("CannedACL", (ObjectCannedACL.to_json f)));
           Aws.Util.option_map v.encryption
             (fun f -> ("Encryption", (Encryption.to_json f)));
           Some ("Prefix", (String.to_json v.prefix));
           Some ("BucketName", (String.to_json v.bucket_name))])
    let of_json j =
      {
        bucket_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "BucketName")));
        prefix =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Prefix")));
        encryption =
          (Aws.Util.option_map (Aws.Json.lookup j "Encryption")
             Encryption.of_json);
        canned_a_c_l =
          (Aws.Util.option_map (Aws.Json.lookup j "CannedACL")
             ObjectCannedACL.of_json);
        access_control_list =
          (Grants.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AccessControlList")));
        tagging =
          (Aws.Util.option_map (Aws.Json.lookup j "Tagging") Tagging.of_json);
        user_metadata =
          (UserMetadata.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "UserMetadata")));
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "StorageClass")
             StorageClass.of_json)
      }
  end
module DeleteBucketLifecycleRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module AllowedOrigins =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module SSES3 =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])
    let to_json v = `Assoc (Aws.Util.list_filter_opt [])
    let of_json j = ()
  end
module SSEKMS =
  struct
    type t = {
      key_id: String.t }
    let make ~key_id  () = { key_id }
    let parse xml =
      Some
        {
          key_id =
            (Aws.Xml.required "KeyId"
               (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml)
                  String.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt [Some ("KeyId", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId")))
      }
  end
module InventoryEncryption =
  struct
    type t = {
      s_s_e_s3: SSES3.t option ;
      s_s_e_k_m_s: SSEKMS.t option }
    let make ?s_s_e_s3  ?s_s_e_k_m_s  () = { s_s_e_s3; s_s_e_k_m_s }
    let parse xml =
      Some
        {
          s_s_e_s3 =
            (Aws.Util.option_bind (Aws.Xml.member "SSE-S3" xml) SSES3.parse);
          s_s_e_k_m_s =
            (Aws.Util.option_bind (Aws.Xml.member "SSE-KMS" xml) SSEKMS.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.s_s_e_k_m_s
              (fun f -> Aws.Query.Pair ("SSE-KMS", (SSEKMS.to_query f)));
           Aws.Util.option_map v.s_s_e_s3
             (fun f -> Aws.Query.Pair ("SSE-S3", (SSES3.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.s_s_e_k_m_s
              (fun f -> ("SSE-KMS", (SSEKMS.to_json f)));
           Aws.Util.option_map v.s_s_e_s3
             (fun f -> ("SSE-S3", (SSES3.to_json f)))])
    let of_json j =
      {
        s_s_e_s3 =
          (Aws.Util.option_map (Aws.Json.lookup j "SSE-S3") SSES3.of_json);
        s_s_e_k_m_s =
          (Aws.Util.option_map (Aws.Json.lookup j "SSE-KMS") SSEKMS.of_json)
      }
  end
module Metadata =
  struct
    type t = (String.t, String.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Aws.Query.to_query_hashtbl String.to_string String.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (String.to_json v)) :: acc)
           v [])
    let of_json j = Aws.Json.to_hashtbl String.of_string String.of_json j
  end
module DeleteMarkerReplicationStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)]
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module AnalyticsS3ExportFileFormat =
  struct
    type t =
      | CSV 
    let str_to_t = [("CSV", CSV)]
    let t_to_str = [(CSV, "CSV")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module BucketAccelerateStatus =
  struct
    type t =
      | Enabled 
      | Suspended 
    let str_to_t = [("Suspended", Suspended); ("Enabled", Enabled)]
    let t_to_str = [(Suspended, "Suspended"); (Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module AccelerateConfiguration =
  struct
    type t = {
      status: BucketAccelerateStatus.t option }
    let make ?status  () = { status }
    let parse xml =
      Some
        {
          status =
            (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
               BucketAccelerateStatus.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.status
              (fun f ->
                 Aws.Query.Pair
                   ("Status", (BucketAccelerateStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.status
              (fun f -> ("Status", (BucketAccelerateStatus.to_json f)))])
    let of_json j =
      {
        status =
          (Aws.Util.option_map (Aws.Json.lookup j "Status")
             BucketAccelerateStatus.of_json)
      }
  end
module JSONOutput =
  struct
    type t = {
      record_delimiter: String.t option }
    let make ?record_delimiter  () = { record_delimiter }
    let parse xml =
      Some
        {
          record_delimiter =
            (Aws.Util.option_bind (Aws.Xml.member "RecordDelimiter" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.record_delimiter
              (fun f ->
                 Aws.Query.Pair ("RecordDelimiter", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.record_delimiter
              (fun f -> ("RecordDelimiter", (String.to_json f)))])
    let of_json j =
      {
        record_delimiter =
          (Aws.Util.option_map (Aws.Json.lookup j "RecordDelimiter")
             String.of_json)
      }
  end
module QuoteFields =
  struct
    type t =
      | ALWAYS 
      | ASNEEDED 
    let str_to_t = [("ASNEEDED", ASNEEDED); ("ALWAYS", ALWAYS)]
    let t_to_str = [(ASNEEDED, "ASNEEDED"); (ALWAYS, "ALWAYS")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module CSVOutput =
  struct
    type t =
      {
      quote_fields: QuoteFields.t option ;
      quote_escape_character: String.t option ;
      record_delimiter: String.t option ;
      field_delimiter: String.t option ;
      quote_character: String.t option }
    let make ?quote_fields  ?quote_escape_character  ?record_delimiter 
      ?field_delimiter  ?quote_character  () =
      {
        quote_fields;
        quote_escape_character;
        record_delimiter;
        field_delimiter;
        quote_character
      }
    let parse xml =
      Some
        {
          quote_fields =
            (Aws.Util.option_bind (Aws.Xml.member "QuoteFields" xml)
               QuoteFields.parse);
          quote_escape_character =
            (Aws.Util.option_bind (Aws.Xml.member "QuoteEscapeCharacter" xml)
               String.parse);
          record_delimiter =
            (Aws.Util.option_bind (Aws.Xml.member "RecordDelimiter" xml)
               String.parse);
          field_delimiter =
            (Aws.Util.option_bind (Aws.Xml.member "FieldDelimiter" xml)
               String.parse);
          quote_character =
            (Aws.Util.option_bind (Aws.Xml.member "QuoteCharacter" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.quote_character
              (fun f ->
                 Aws.Query.Pair ("QuoteCharacter", (String.to_query f)));
           Aws.Util.option_map v.field_delimiter
             (fun f -> Aws.Query.Pair ("FieldDelimiter", (String.to_query f)));
           Aws.Util.option_map v.record_delimiter
             (fun f ->
                Aws.Query.Pair ("RecordDelimiter", (String.to_query f)));
           Aws.Util.option_map v.quote_escape_character
             (fun f ->
                Aws.Query.Pair ("QuoteEscapeCharacter", (String.to_query f)));
           Aws.Util.option_map v.quote_fields
             (fun f ->
                Aws.Query.Pair ("QuoteFields", (QuoteFields.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.quote_character
              (fun f -> ("QuoteCharacter", (String.to_json f)));
           Aws.Util.option_map v.field_delimiter
             (fun f -> ("FieldDelimiter", (String.to_json f)));
           Aws.Util.option_map v.record_delimiter
             (fun f -> ("RecordDelimiter", (String.to_json f)));
           Aws.Util.option_map v.quote_escape_character
             (fun f -> ("QuoteEscapeCharacter", (String.to_json f)));
           Aws.Util.option_map v.quote_fields
             (fun f -> ("QuoteFields", (QuoteFields.to_json f)))])
    let of_json j =
      {
        quote_fields =
          (Aws.Util.option_map (Aws.Json.lookup j "QuoteFields")
             QuoteFields.of_json);
        quote_escape_character =
          (Aws.Util.option_map (Aws.Json.lookup j "QuoteEscapeCharacter")
             String.of_json);
        record_delimiter =
          (Aws.Util.option_map (Aws.Json.lookup j "RecordDelimiter")
             String.of_json);
        field_delimiter =
          (Aws.Util.option_map (Aws.Json.lookup j "FieldDelimiter")
             String.of_json);
        quote_character =
          (Aws.Util.option_map (Aws.Json.lookup j "QuoteCharacter")
             String.of_json)
      }
  end
module OutputSerialization =
  struct
    type t = {
      c_s_v: CSVOutput.t option ;
      j_s_o_n: JSONOutput.t option }
    let make ?c_s_v  ?j_s_o_n  () = { c_s_v; j_s_o_n }
    let parse xml =
      Some
        {
          c_s_v =
            (Aws.Util.option_bind (Aws.Xml.member "CSV" xml) CSVOutput.parse);
          j_s_o_n =
            (Aws.Util.option_bind (Aws.Xml.member "JSON" xml)
               JSONOutput.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.j_s_o_n
              (fun f -> Aws.Query.Pair ("JSON", (JSONOutput.to_query f)));
           Aws.Util.option_map v.c_s_v
             (fun f -> Aws.Query.Pair ("CSV", (CSVOutput.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.j_s_o_n
              (fun f -> ("JSON", (JSONOutput.to_json f)));
           Aws.Util.option_map v.c_s_v
             (fun f -> ("CSV", (CSVOutput.to_json f)))])
    let of_json j =
      {
        c_s_v =
          (Aws.Util.option_map (Aws.Json.lookup j "CSV") CSVOutput.of_json);
        j_s_o_n =
          (Aws.Util.option_map (Aws.Json.lookup j "JSON") JSONOutput.of_json)
      }
  end
module AllowedMethods =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module BucketLogsPermission =
  struct
    type t =
      | FULL_CONTROL 
      | READ 
      | WRITE 
    let str_to_t =
      [("WRITE", WRITE); ("READ", READ); ("FULL_CONTROL", FULL_CONTROL)]
    let t_to_str =
      [(WRITE, "WRITE"); (READ, "READ"); (FULL_CONTROL, "FULL_CONTROL")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module TargetGrant =
  struct
    type t =
      {
      grantee: Grantee.t option ;
      permission: BucketLogsPermission.t option }
    let make ?grantee  ?permission  () = { grantee; permission }
    let parse xml =
      Some
        {
          grantee =
            (Aws.Util.option_bind (Aws.Xml.member "Grantee" xml)
               Grantee.parse);
          permission =
            (Aws.Util.option_bind (Aws.Xml.member "Permission" xml)
               BucketLogsPermission.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.permission
              (fun f ->
                 Aws.Query.Pair
                   ("Permission", (BucketLogsPermission.to_query f)));
           Aws.Util.option_map v.grantee
             (fun f -> Aws.Query.Pair ("Grantee", (Grantee.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.permission
              (fun f -> ("Permission", (BucketLogsPermission.to_json f)));
           Aws.Util.option_map v.grantee
             (fun f -> ("Grantee", (Grantee.to_json f)))])
    let of_json j =
      {
        grantee =
          (Aws.Util.option_map (Aws.Json.lookup j "Grantee") Grantee.of_json);
        permission =
          (Aws.Util.option_map (Aws.Json.lookup j "Permission")
             BucketLogsPermission.of_json)
      }
  end
module TargetGrants =
  struct
    type t = TargetGrant.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map TargetGrant.parse (Aws.Xml.members "Grant" xml))
    let to_query v = Aws.Query.to_query_list TargetGrant.to_query v
    let to_json v = `List (List.map TargetGrant.to_json v)
    let of_json j = Aws.Json.to_list TargetGrant.of_json j
  end
module LoggingEnabled =
  struct
    type t =
      {
      target_bucket: String.t ;
      target_grants: TargetGrants.t ;
      target_prefix: String.t }
    let make ~target_bucket  ?(target_grants= [])  ~target_prefix  () =
      { target_bucket; target_grants; target_prefix }
    let parse xml =
      Some
        {
          target_bucket =
            (Aws.Xml.required "TargetBucket"
               (Aws.Util.option_bind (Aws.Xml.member "TargetBucket" xml)
                  String.parse));
          target_grants =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "TargetGrants" xml)
                  TargetGrants.parse));
          target_prefix =
            (Aws.Xml.required "TargetPrefix"
               (Aws.Util.option_bind (Aws.Xml.member "TargetPrefix" xml)
                  String.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("TargetPrefix", (String.to_query v.target_prefix)));
           Some
             (Aws.Query.Pair
                ("TargetGrants.member",
                  (TargetGrants.to_query v.target_grants)));
           Some
             (Aws.Query.Pair
                ("TargetBucket", (String.to_query v.target_bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("TargetPrefix", (String.to_json v.target_prefix));
           Some ("TargetGrants", (TargetGrants.to_json v.target_grants));
           Some ("TargetBucket", (String.to_json v.target_bucket))])
    let of_json j =
      {
        target_bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetBucket")));
        target_grants =
          (TargetGrants.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetGrants")));
        target_prefix =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetPrefix")))
      }
  end
module FilterRuleName =
  struct
    type t =
      | Prefix 
      | Suffix 
    let str_to_t = [("suffix", Suffix); ("prefix", Prefix)]
    let t_to_str = [(Suffix, "suffix"); (Prefix, "prefix")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module FilterRule =
  struct
    type t = {
      name: FilterRuleName.t option ;
      value: String.t option }
    let make ?name  ?value  () = { name; value }
    let parse xml =
      Some
        {
          name =
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml)
               FilterRuleName.parse);
          value =
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.value
              (fun f -> Aws.Query.Pair ("Value", (String.to_query f)));
           Aws.Util.option_map v.name
             (fun f -> Aws.Query.Pair ("Name", (FilterRuleName.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.value
              (fun f -> ("Value", (String.to_json f)));
           Aws.Util.option_map v.name
             (fun f -> ("Name", (FilterRuleName.to_json f)))])
    let of_json j =
      {
        name =
          (Aws.Util.option_map (Aws.Json.lookup j "Name")
             FilterRuleName.of_json);
        value =
          (Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json)
      }
  end
module FilterRuleList =
  struct
    type t = FilterRule.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map FilterRule.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list FilterRule.to_query v
    let to_json v = `List (List.map FilterRule.to_json v)
    let of_json j = Aws.Json.to_list FilterRule.of_json j
  end
module S3KeyFilter =
  struct
    type t = {
      filter_rules: FilterRuleList.t }
    let make ?(filter_rules= [])  () = { filter_rules }
    let parse xml =
      Some
        { filter_rules = (Aws.Util.of_option [] (FilterRuleList.parse xml)) }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("FilterRule", (FilterRuleList.to_query v.filter_rules)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("FilterRule", (FilterRuleList.to_json v.filter_rules))])
    let of_json j =
      {
        filter_rules =
          (FilterRuleList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "FilterRule")))
      }
  end
module NotificationConfigurationFilter =
  struct
    type t = {
      key: S3KeyFilter.t option }
    let make ?key  () = { key }
    let parse xml =
      Some
        {
          key =
            (Aws.Util.option_bind (Aws.Xml.member "S3Key" xml)
               S3KeyFilter.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.key
              (fun f -> Aws.Query.Pair ("S3Key", (S3KeyFilter.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.key
              (fun f -> ("S3Key", (S3KeyFilter.to_json f)))])
    let of_json j =
      {
        key =
          (Aws.Util.option_map (Aws.Json.lookup j "S3Key")
             S3KeyFilter.of_json)
      }
  end
module Event =
  struct
    type t =
      | S3_ReducedRedundancyLostObject 
      | S3_ObjectCreated__ 
      | S3_ObjectCreated_Put 
      | S3_ObjectCreated_Post 
      | S3_ObjectCreated_Copy 
      | S3_ObjectCreated_CompleteMultipartUpload 
      | S3_ObjectRemoved__ 
      | S3_ObjectRemoved_Delete 
      | S3_ObjectRemoved_DeleteMarkerCreated 
      | S3_ObjectRestore__ 
      | S3_ObjectRestore_Post 
      | S3_ObjectRestore_Completed 
      | S3_Replication__ 
      | S3_Replication_OperationFailedReplication 
      | S3_Replication_OperationNotTracked 
      | S3_Replication_OperationMissedThreshold 
      | S3_Replication_OperationReplicatedAfterThreshold 
      | S3_ObjectRestore_Delete 
      | S3_LifecycleTransition 
      | S3_IntelligentTiering 
      | S3_ObjectAcl_Put 
      | S3_LifecycleExpiration__ 
      | S3_LifecycleExpiration_Delete 
      | S3_LifecycleExpiration_DeleteMarkerCreated 
      | S3_ObjectTagging__ 
      | S3_ObjectTagging_Put 
      | S3_ObjectTagging_Delete 
    let str_to_t =
      [("s3:ObjectTagging:Delete", S3_ObjectTagging_Delete);
      ("s3:ObjectTagging:Put", S3_ObjectTagging_Put);
      ("s3:ObjectTagging:*", S3_ObjectTagging__);
      ("s3:LifecycleExpiration:DeleteMarkerCreated",
        S3_LifecycleExpiration_DeleteMarkerCreated);
      ("s3:LifecycleExpiration:Delete", S3_LifecycleExpiration_Delete);
      ("s3:LifecycleExpiration:*", S3_LifecycleExpiration__);
      ("s3:ObjectAcl:Put", S3_ObjectAcl_Put);
      ("s3:IntelligentTiering", S3_IntelligentTiering);
      ("s3:LifecycleTransition", S3_LifecycleTransition);
      ("s3:ObjectRestore:Delete", S3_ObjectRestore_Delete);
      ("s3:Replication:OperationReplicatedAfterThreshold",
        S3_Replication_OperationReplicatedAfterThreshold);
      ("s3:Replication:OperationMissedThreshold",
        S3_Replication_OperationMissedThreshold);
      ("s3:Replication:OperationNotTracked",
        S3_Replication_OperationNotTracked);
      ("s3:Replication:OperationFailedReplication",
        S3_Replication_OperationFailedReplication);
      ("s3:Replication:*", S3_Replication__);
      ("s3:ObjectRestore:Completed", S3_ObjectRestore_Completed);
      ("s3:ObjectRestore:Post", S3_ObjectRestore_Post);
      ("s3:ObjectRestore:*", S3_ObjectRestore__);
      ("s3:ObjectRemoved:DeleteMarkerCreated",
        S3_ObjectRemoved_DeleteMarkerCreated);
      ("s3:ObjectRemoved:Delete", S3_ObjectRemoved_Delete);
      ("s3:ObjectRemoved:*", S3_ObjectRemoved__);
      ("s3:ObjectCreated:CompleteMultipartUpload",
        S3_ObjectCreated_CompleteMultipartUpload);
      ("s3:ObjectCreated:Copy", S3_ObjectCreated_Copy);
      ("s3:ObjectCreated:Post", S3_ObjectCreated_Post);
      ("s3:ObjectCreated:Put", S3_ObjectCreated_Put);
      ("s3:ObjectCreated:*", S3_ObjectCreated__);
      ("s3:ReducedRedundancyLostObject", S3_ReducedRedundancyLostObject)]
    let t_to_str =
      [(S3_ObjectTagging_Delete, "s3:ObjectTagging:Delete");
      (S3_ObjectTagging_Put, "s3:ObjectTagging:Put");
      (S3_ObjectTagging__, "s3:ObjectTagging:*");
      (S3_LifecycleExpiration_DeleteMarkerCreated,
        "s3:LifecycleExpiration:DeleteMarkerCreated");
      (S3_LifecycleExpiration_Delete, "s3:LifecycleExpiration:Delete");
      (S3_LifecycleExpiration__, "s3:LifecycleExpiration:*");
      (S3_ObjectAcl_Put, "s3:ObjectAcl:Put");
      (S3_IntelligentTiering, "s3:IntelligentTiering");
      (S3_LifecycleTransition, "s3:LifecycleTransition");
      (S3_ObjectRestore_Delete, "s3:ObjectRestore:Delete");
      (S3_Replication_OperationReplicatedAfterThreshold,
        "s3:Replication:OperationReplicatedAfterThreshold");
      (S3_Replication_OperationMissedThreshold,
        "s3:Replication:OperationMissedThreshold");
      (S3_Replication_OperationNotTracked,
        "s3:Replication:OperationNotTracked");
      (S3_Replication_OperationFailedReplication,
        "s3:Replication:OperationFailedReplication");
      (S3_Replication__, "s3:Replication:*");
      (S3_ObjectRestore_Completed, "s3:ObjectRestore:Completed");
      (S3_ObjectRestore_Post, "s3:ObjectRestore:Post");
      (S3_ObjectRestore__, "s3:ObjectRestore:*");
      (S3_ObjectRemoved_DeleteMarkerCreated,
        "s3:ObjectRemoved:DeleteMarkerCreated");
      (S3_ObjectRemoved_Delete, "s3:ObjectRemoved:Delete");
      (S3_ObjectRemoved__, "s3:ObjectRemoved:*");
      (S3_ObjectCreated_CompleteMultipartUpload,
        "s3:ObjectCreated:CompleteMultipartUpload");
      (S3_ObjectCreated_Copy, "s3:ObjectCreated:Copy");
      (S3_ObjectCreated_Post, "s3:ObjectCreated:Post");
      (S3_ObjectCreated_Put, "s3:ObjectCreated:Put");
      (S3_ObjectCreated__, "s3:ObjectCreated:*");
      (S3_ReducedRedundancyLostObject, "s3:ReducedRedundancyLostObject")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module EventList =
  struct
    type t = Event.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map Event.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list Event.to_query v
    let to_json v = `List (List.map Event.to_json v)
    let of_json j = Aws.Json.to_list Event.of_json j
  end
module TopicConfiguration =
  struct
    type t =
      {
      id: String.t option ;
      topic_arn: String.t ;
      events: EventList.t ;
      filter: NotificationConfigurationFilter.t option }
    let make ?id  ~topic_arn  ~events  ?filter  () =
      { id; topic_arn; events; filter }
    let parse xml =
      Some
        {
          id = (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse);
          topic_arn =
            (Aws.Xml.required "Topic"
               (Aws.Util.option_bind (Aws.Xml.member "Topic" xml)
                  String.parse));
          events = (Aws.Xml.required "Event" (EventList.parse xml));
          filter =
            (Aws.Util.option_bind (Aws.Xml.member "Filter" xml)
               NotificationConfigurationFilter.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.filter
              (fun f ->
                 Aws.Query.Pair
                   ("Filter", (NotificationConfigurationFilter.to_query f)));
           Some (Aws.Query.Pair ("Event", (EventList.to_query v.events)));
           Some (Aws.Query.Pair ("Topic", (String.to_query v.topic_arn)));
           Aws.Util.option_map v.id
             (fun f -> Aws.Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.filter
              (fun f ->
                 ("Filter", (NotificationConfigurationFilter.to_json f)));
           Some ("Event", (EventList.to_json v.events));
           Some ("Topic", (String.to_json v.topic_arn));
           Aws.Util.option_map v.id (fun f -> ("Id", (String.to_json f)))])
    let of_json j =
      {
        id = (Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json);
        topic_arn =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Topic")));
        events =
          (EventList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Event")));
        filter =
          (Aws.Util.option_map (Aws.Json.lookup j "Filter")
             NotificationConfigurationFilter.of_json)
      }
  end
module TopicConfigurationList =
  struct
    type t = TopicConfiguration.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map TopicConfiguration.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list TopicConfiguration.to_query v
    let to_json v = `List (List.map TopicConfiguration.to_json v)
    let of_json j = Aws.Json.to_list TopicConfiguration.of_json j
  end
module QueueConfiguration =
  struct
    type t =
      {
      id: String.t option ;
      queue_arn: String.t ;
      events: EventList.t ;
      filter: NotificationConfigurationFilter.t option }
    let make ?id  ~queue_arn  ~events  ?filter  () =
      { id; queue_arn; events; filter }
    let parse xml =
      Some
        {
          id = (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse);
          queue_arn =
            (Aws.Xml.required "Queue"
               (Aws.Util.option_bind (Aws.Xml.member "Queue" xml)
                  String.parse));
          events = (Aws.Xml.required "Event" (EventList.parse xml));
          filter =
            (Aws.Util.option_bind (Aws.Xml.member "Filter" xml)
               NotificationConfigurationFilter.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.filter
              (fun f ->
                 Aws.Query.Pair
                   ("Filter", (NotificationConfigurationFilter.to_query f)));
           Some (Aws.Query.Pair ("Event", (EventList.to_query v.events)));
           Some (Aws.Query.Pair ("Queue", (String.to_query v.queue_arn)));
           Aws.Util.option_map v.id
             (fun f -> Aws.Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.filter
              (fun f ->
                 ("Filter", (NotificationConfigurationFilter.to_json f)));
           Some ("Event", (EventList.to_json v.events));
           Some ("Queue", (String.to_json v.queue_arn));
           Aws.Util.option_map v.id (fun f -> ("Id", (String.to_json f)))])
    let of_json j =
      {
        id = (Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json);
        queue_arn =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Queue")));
        events =
          (EventList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Event")));
        filter =
          (Aws.Util.option_map (Aws.Json.lookup j "Filter")
             NotificationConfigurationFilter.of_json)
      }
  end
module QueueConfigurationList =
  struct
    type t = QueueConfiguration.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map QueueConfiguration.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list QueueConfiguration.to_query v
    let to_json v = `List (List.map QueueConfiguration.to_json v)
    let of_json j = Aws.Json.to_list QueueConfiguration.of_json j
  end
module LambdaFunctionConfiguration =
  struct
    type t =
      {
      id: String.t option ;
      lambda_function_arn: String.t ;
      events: EventList.t ;
      filter: NotificationConfigurationFilter.t option }
    let make ?id  ~lambda_function_arn  ~events  ?filter  () =
      { id; lambda_function_arn; events; filter }
    let parse xml =
      Some
        {
          id = (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse);
          lambda_function_arn =
            (Aws.Xml.required "CloudFunction"
               (Aws.Util.option_bind (Aws.Xml.member "CloudFunction" xml)
                  String.parse));
          events = (Aws.Xml.required "Event" (EventList.parse xml));
          filter =
            (Aws.Util.option_bind (Aws.Xml.member "Filter" xml)
               NotificationConfigurationFilter.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.filter
              (fun f ->
                 Aws.Query.Pair
                   ("Filter", (NotificationConfigurationFilter.to_query f)));
           Some (Aws.Query.Pair ("Event", (EventList.to_query v.events)));
           Some
             (Aws.Query.Pair
                ("CloudFunction", (String.to_query v.lambda_function_arn)));
           Aws.Util.option_map v.id
             (fun f -> Aws.Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.filter
              (fun f ->
                 ("Filter", (NotificationConfigurationFilter.to_json f)));
           Some ("Event", (EventList.to_json v.events));
           Some ("CloudFunction", (String.to_json v.lambda_function_arn));
           Aws.Util.option_map v.id (fun f -> ("Id", (String.to_json f)))])
    let of_json j =
      {
        id = (Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json);
        lambda_function_arn =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "CloudFunction")));
        events =
          (EventList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Event")));
        filter =
          (Aws.Util.option_map (Aws.Json.lookup j "Filter")
             NotificationConfigurationFilter.of_json)
      }
  end
module LambdaFunctionConfigurationList =
  struct
    type t = LambdaFunctionConfiguration.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map LambdaFunctionConfiguration.parse
           (Aws.Xml.members "member" xml))
    let to_query v =
      Aws.Query.to_query_list LambdaFunctionConfiguration.to_query v
    let to_json v = `List (List.map LambdaFunctionConfiguration.to_json v)
    let of_json j = Aws.Json.to_list LambdaFunctionConfiguration.of_json j
  end
module EventBridgeConfiguration =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])
    let to_json v = `Assoc (Aws.Util.list_filter_opt [])
    let of_json j = ()
  end
module NotificationConfiguration =
  struct
    type t =
      {
      topic_configurations: TopicConfigurationList.t ;
      queue_configurations: QueueConfigurationList.t ;
      lambda_function_configurations: LambdaFunctionConfigurationList.t ;
      event_bridge_configuration: EventBridgeConfiguration.t option }
    let make ?(topic_configurations= [])  ?(queue_configurations= []) 
      ?(lambda_function_configurations= [])  ?event_bridge_configuration  ()
      =
      {
        topic_configurations;
        queue_configurations;
        lambda_function_configurations;
        event_bridge_configuration
      }
    let parse xml =
      Some
        {
          topic_configurations =
            (Aws.Util.of_option [] (TopicConfigurationList.parse xml));
          queue_configurations =
            (Aws.Util.of_option [] (QueueConfigurationList.parse xml));
          lambda_function_configurations =
            (Aws.Util.of_option []
               (LambdaFunctionConfigurationList.parse xml));
          event_bridge_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "EventBridgeConfiguration" xml)
               EventBridgeConfiguration.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.event_bridge_configuration
              (fun f ->
                 Aws.Query.Pair
                   ("EventBridgeConfiguration",
                     (EventBridgeConfiguration.to_query f)));
           Some
             (Aws.Query.Pair
                ("CloudFunctionConfiguration",
                  (LambdaFunctionConfigurationList.to_query
                     v.lambda_function_configurations)));
           Some
             (Aws.Query.Pair
                ("QueueConfiguration",
                  (QueueConfigurationList.to_query v.queue_configurations)));
           Some
             (Aws.Query.Pair
                ("TopicConfiguration",
                  (TopicConfigurationList.to_query v.topic_configurations)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.event_bridge_configuration
              (fun f ->
                 ("EventBridgeConfiguration",
                   (EventBridgeConfiguration.to_json f)));
           Some
             ("CloudFunctionConfiguration",
               (LambdaFunctionConfigurationList.to_json
                  v.lambda_function_configurations));
           Some
             ("QueueConfiguration",
               (QueueConfigurationList.to_json v.queue_configurations));
           Some
             ("TopicConfiguration",
               (TopicConfigurationList.to_json v.topic_configurations))])
    let of_json j =
      {
        topic_configurations =
          (TopicConfigurationList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TopicConfiguration")));
        queue_configurations =
          (QueueConfigurationList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "QueueConfiguration")));
        lambda_function_configurations =
          (LambdaFunctionConfigurationList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "CloudFunctionConfiguration")));
        event_bridge_configuration =
          (Aws.Util.option_map (Aws.Json.lookup j "EventBridgeConfiguration")
             EventBridgeConfiguration.of_json)
      }
  end
module PutBucketNotificationConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      notification_configuration: NotificationConfiguration.t ;
      expected_bucket_owner: String.t option ;
      skip_destination_validation: Boolean.t option }
    let make ~bucket  ~notification_configuration  ?expected_bucket_owner 
      ?skip_destination_validation  () =
      {
        bucket;
        notification_configuration;
        expected_bucket_owner;
        skip_destination_validation
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          notification_configuration =
            (Aws.Xml.required "NotificationConfiguration"
               (Aws.Util.option_bind
                  (Aws.Xml.member "NotificationConfiguration" xml)
                  NotificationConfiguration.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          skip_destination_validation =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-skip-destination-validation" xml)
               Boolean.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.skip_destination_validation
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-skip-destination-validation",
                     (Boolean.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("NotificationConfiguration",
                  (NotificationConfiguration.to_query
                     v.notification_configuration)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.skip_destination_validation
              (fun f ->
                 ("x-amz-skip-destination-validation", (Boolean.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some
             ("NotificationConfiguration",
               (NotificationConfiguration.to_json
                  v.notification_configuration));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        notification_configuration =
          (NotificationConfiguration.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "NotificationConfiguration")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        skip_destination_validation =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-skip-destination-validation")
             Boolean.of_json)
      }
  end
module TransitionStorageClass =
  struct
    type t =
      | GLACIER 
      | STANDARD_IA 
      | ONEZONE_IA 
      | INTELLIGENT_TIERING 
      | DEEP_ARCHIVE 
      | GLACIER_IR 
    let str_to_t =
      [("GLACIER_IR", GLACIER_IR);
      ("DEEP_ARCHIVE", DEEP_ARCHIVE);
      ("INTELLIGENT_TIERING", INTELLIGENT_TIERING);
      ("ONEZONE_IA", ONEZONE_IA);
      ("STANDARD_IA", STANDARD_IA);
      ("GLACIER", GLACIER)]
    let t_to_str =
      [(GLACIER_IR, "GLACIER_IR");
      (DEEP_ARCHIVE, "DEEP_ARCHIVE");
      (INTELLIGENT_TIERING, "INTELLIGENT_TIERING");
      (ONEZONE_IA, "ONEZONE_IA");
      (STANDARD_IA, "STANDARD_IA");
      (GLACIER, "GLACIER")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module Transition =
  struct
    type t =
      {
      date: DateTime.t option ;
      days: Integer.t option ;
      storage_class: TransitionStorageClass.t option }
    let make ?date  ?days  ?storage_class  () = { date; days; storage_class }
    let parse xml =
      Some
        {
          date =
            (Aws.Util.option_bind (Aws.Xml.member "Date" xml) DateTime.parse);
          days =
            (Aws.Util.option_bind (Aws.Xml.member "Days" xml) Integer.parse);
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "StorageClass" xml)
               TransitionStorageClass.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.storage_class
              (fun f ->
                 Aws.Query.Pair
                   ("StorageClass", (TransitionStorageClass.to_query f)));
           Aws.Util.option_map v.days
             (fun f -> Aws.Query.Pair ("Days", (Integer.to_query f)));
           Aws.Util.option_map v.date
             (fun f -> Aws.Query.Pair ("Date", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.storage_class
              (fun f -> ("StorageClass", (TransitionStorageClass.to_json f)));
           Aws.Util.option_map v.days
             (fun f -> ("Days", (Integer.to_json f)));
           Aws.Util.option_map v.date
             (fun f -> ("Date", (DateTime.to_json f)))])
    let of_json j =
      {
        date =
          (Aws.Util.option_map (Aws.Json.lookup j "Date") DateTime.of_json);
        days =
          (Aws.Util.option_map (Aws.Json.lookup j "Days") Integer.of_json);
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "StorageClass")
             TransitionStorageClass.of_json)
      }
  end
module NoncurrentVersionTransition =
  struct
    type t =
      {
      noncurrent_days: Integer.t option ;
      storage_class: TransitionStorageClass.t option ;
      newer_noncurrent_versions: Integer.t option }
    let make ?noncurrent_days  ?storage_class  ?newer_noncurrent_versions  ()
      = { noncurrent_days; storage_class; newer_noncurrent_versions }
    let parse xml =
      Some
        {
          noncurrent_days =
            (Aws.Util.option_bind (Aws.Xml.member "NoncurrentDays" xml)
               Integer.parse);
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "StorageClass" xml)
               TransitionStorageClass.parse);
          newer_noncurrent_versions =
            (Aws.Util.option_bind
               (Aws.Xml.member "NewerNoncurrentVersions" xml) Integer.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.newer_noncurrent_versions
              (fun f ->
                 Aws.Query.Pair
                   ("NewerNoncurrentVersions", (Integer.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair
                  ("StorageClass", (TransitionStorageClass.to_query f)));
           Aws.Util.option_map v.noncurrent_days
             (fun f ->
                Aws.Query.Pair ("NoncurrentDays", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.newer_noncurrent_versions
              (fun f -> ("NewerNoncurrentVersions", (Integer.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f -> ("StorageClass", (TransitionStorageClass.to_json f)));
           Aws.Util.option_map v.noncurrent_days
             (fun f -> ("NoncurrentDays", (Integer.to_json f)))])
    let of_json j =
      {
        noncurrent_days =
          (Aws.Util.option_map (Aws.Json.lookup j "NoncurrentDays")
             Integer.of_json);
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "StorageClass")
             TransitionStorageClass.of_json);
        newer_noncurrent_versions =
          (Aws.Util.option_map (Aws.Json.lookup j "NewerNoncurrentVersions")
             Integer.of_json)
      }
  end
module NoncurrentVersionExpiration =
  struct
    type t =
      {
      noncurrent_days: Integer.t option ;
      newer_noncurrent_versions: Integer.t option }
    let make ?noncurrent_days  ?newer_noncurrent_versions  () =
      { noncurrent_days; newer_noncurrent_versions }
    let parse xml =
      Some
        {
          noncurrent_days =
            (Aws.Util.option_bind (Aws.Xml.member "NoncurrentDays" xml)
               Integer.parse);
          newer_noncurrent_versions =
            (Aws.Util.option_bind
               (Aws.Xml.member "NewerNoncurrentVersions" xml) Integer.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.newer_noncurrent_versions
              (fun f ->
                 Aws.Query.Pair
                   ("NewerNoncurrentVersions", (Integer.to_query f)));
           Aws.Util.option_map v.noncurrent_days
             (fun f ->
                Aws.Query.Pair ("NoncurrentDays", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.newer_noncurrent_versions
              (fun f -> ("NewerNoncurrentVersions", (Integer.to_json f)));
           Aws.Util.option_map v.noncurrent_days
             (fun f -> ("NoncurrentDays", (Integer.to_json f)))])
    let of_json j =
      {
        noncurrent_days =
          (Aws.Util.option_map (Aws.Json.lookup j "NoncurrentDays")
             Integer.of_json);
        newer_noncurrent_versions =
          (Aws.Util.option_map (Aws.Json.lookup j "NewerNoncurrentVersions")
             Integer.of_json)
      }
  end
module LifecycleExpiration =
  struct
    type t =
      {
      date: DateTime.t option ;
      days: Integer.t option ;
      expired_object_delete_marker: Boolean.t option }
    let make ?date  ?days  ?expired_object_delete_marker  () =
      { date; days; expired_object_delete_marker }
    let parse xml =
      Some
        {
          date =
            (Aws.Util.option_bind (Aws.Xml.member "Date" xml) DateTime.parse);
          days =
            (Aws.Util.option_bind (Aws.Xml.member "Days" xml) Integer.parse);
          expired_object_delete_marker =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExpiredObjectDeleteMarker" xml) Boolean.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expired_object_delete_marker
              (fun f ->
                 Aws.Query.Pair
                   ("ExpiredObjectDeleteMarker", (Boolean.to_query f)));
           Aws.Util.option_map v.days
             (fun f -> Aws.Query.Pair ("Days", (Integer.to_query f)));
           Aws.Util.option_map v.date
             (fun f -> Aws.Query.Pair ("Date", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expired_object_delete_marker
              (fun f -> ("ExpiredObjectDeleteMarker", (Boolean.to_json f)));
           Aws.Util.option_map v.days
             (fun f -> ("Days", (Integer.to_json f)));
           Aws.Util.option_map v.date
             (fun f -> ("Date", (DateTime.to_json f)))])
    let of_json j =
      {
        date =
          (Aws.Util.option_map (Aws.Json.lookup j "Date") DateTime.of_json);
        days =
          (Aws.Util.option_map (Aws.Json.lookup j "Days") Integer.of_json);
        expired_object_delete_marker =
          (Aws.Util.option_map
             (Aws.Json.lookup j "ExpiredObjectDeleteMarker") Boolean.of_json)
      }
  end
module ExpirationStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)]
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module AbortIncompleteMultipartUpload =
  struct
    type t = {
      days_after_initiation: Integer.t option }
    let make ?days_after_initiation  () = { days_after_initiation }
    let parse xml =
      Some
        {
          days_after_initiation =
            (Aws.Util.option_bind (Aws.Xml.member "DaysAfterInitiation" xml)
               Integer.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.days_after_initiation
              (fun f ->
                 Aws.Query.Pair ("DaysAfterInitiation", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.days_after_initiation
              (fun f -> ("DaysAfterInitiation", (Integer.to_json f)))])
    let of_json j =
      {
        days_after_initiation =
          (Aws.Util.option_map (Aws.Json.lookup j "DaysAfterInitiation")
             Integer.of_json)
      }
  end
module Rule =
  struct
    type t =
      {
      expiration: LifecycleExpiration.t option ;
      i_d: String.t option ;
      prefix: String.t ;
      status: ExpirationStatus.t ;
      transition: Transition.t option ;
      noncurrent_version_transition: NoncurrentVersionTransition.t option ;
      noncurrent_version_expiration: NoncurrentVersionExpiration.t option ;
      abort_incomplete_multipart_upload:
        AbortIncompleteMultipartUpload.t option }
    let make ?expiration  ?i_d  ~prefix  ~status  ?transition 
      ?noncurrent_version_transition  ?noncurrent_version_expiration 
      ?abort_incomplete_multipart_upload  () =
      {
        expiration;
        i_d;
        prefix;
        status;
        transition;
        noncurrent_version_transition;
        noncurrent_version_expiration;
        abort_incomplete_multipart_upload
      }
    let parse xml =
      Some
        {
          expiration =
            (Aws.Util.option_bind (Aws.Xml.member "Expiration" xml)
               LifecycleExpiration.parse);
          i_d = (Aws.Util.option_bind (Aws.Xml.member "ID" xml) String.parse);
          prefix =
            (Aws.Xml.required "Prefix"
               (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml)
                  String.parse));
          status =
            (Aws.Xml.required "Status"
               (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
                  ExpirationStatus.parse));
          transition =
            (Aws.Util.option_bind (Aws.Xml.member "Transition" xml)
               Transition.parse);
          noncurrent_version_transition =
            (Aws.Util.option_bind
               (Aws.Xml.member "NoncurrentVersionTransition" xml)
               NoncurrentVersionTransition.parse);
          noncurrent_version_expiration =
            (Aws.Util.option_bind
               (Aws.Xml.member "NoncurrentVersionExpiration" xml)
               NoncurrentVersionExpiration.parse);
          abort_incomplete_multipart_upload =
            (Aws.Util.option_bind
               (Aws.Xml.member "AbortIncompleteMultipartUpload" xml)
               AbortIncompleteMultipartUpload.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.abort_incomplete_multipart_upload
              (fun f ->
                 Aws.Query.Pair
                   ("AbortIncompleteMultipartUpload",
                     (AbortIncompleteMultipartUpload.to_query f)));
           Aws.Util.option_map v.noncurrent_version_expiration
             (fun f ->
                Aws.Query.Pair
                  ("NoncurrentVersionExpiration",
                    (NoncurrentVersionExpiration.to_query f)));
           Aws.Util.option_map v.noncurrent_version_transition
             (fun f ->
                Aws.Query.Pair
                  ("NoncurrentVersionTransition",
                    (NoncurrentVersionTransition.to_query f)));
           Aws.Util.option_map v.transition
             (fun f -> Aws.Query.Pair ("Transition", (Transition.to_query f)));
           Some
             (Aws.Query.Pair ("Status", (ExpirationStatus.to_query v.status)));
           Some (Aws.Query.Pair ("Prefix", (String.to_query v.prefix)));
           Aws.Util.option_map v.i_d
             (fun f -> Aws.Query.Pair ("ID", (String.to_query f)));
           Aws.Util.option_map v.expiration
             (fun f ->
                Aws.Query.Pair
                  ("Expiration", (LifecycleExpiration.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.abort_incomplete_multipart_upload
              (fun f ->
                 ("AbortIncompleteMultipartUpload",
                   (AbortIncompleteMultipartUpload.to_json f)));
           Aws.Util.option_map v.noncurrent_version_expiration
             (fun f ->
                ("NoncurrentVersionExpiration",
                  (NoncurrentVersionExpiration.to_json f)));
           Aws.Util.option_map v.noncurrent_version_transition
             (fun f ->
                ("NoncurrentVersionTransition",
                  (NoncurrentVersionTransition.to_json f)));
           Aws.Util.option_map v.transition
             (fun f -> ("Transition", (Transition.to_json f)));
           Some ("Status", (ExpirationStatus.to_json v.status));
           Some ("Prefix", (String.to_json v.prefix));
           Aws.Util.option_map v.i_d (fun f -> ("ID", (String.to_json f)));
           Aws.Util.option_map v.expiration
             (fun f -> ("Expiration", (LifecycleExpiration.to_json f)))])
    let of_json j =
      {
        expiration =
          (Aws.Util.option_map (Aws.Json.lookup j "Expiration")
             LifecycleExpiration.of_json);
        i_d = (Aws.Util.option_map (Aws.Json.lookup j "ID") String.of_json);
        prefix =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Prefix")));
        status =
          (ExpirationStatus.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Status")));
        transition =
          (Aws.Util.option_map (Aws.Json.lookup j "Transition")
             Transition.of_json);
        noncurrent_version_transition =
          (Aws.Util.option_map
             (Aws.Json.lookup j "NoncurrentVersionTransition")
             NoncurrentVersionTransition.of_json);
        noncurrent_version_expiration =
          (Aws.Util.option_map
             (Aws.Json.lookup j "NoncurrentVersionExpiration")
             NoncurrentVersionExpiration.of_json);
        abort_incomplete_multipart_upload =
          (Aws.Util.option_map
             (Aws.Json.lookup j "AbortIncompleteMultipartUpload")
             AbortIncompleteMultipartUpload.of_json)
      }
  end
module Rules =
  struct
    type t = Rule.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map Rule.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list Rule.to_query v
    let to_json v = `List (List.map Rule.to_json v)
    let of_json j = Aws.Json.to_list Rule.of_json j
  end
module LifecycleConfiguration =
  struct
    type t = {
      rules: Rules.t }
    let make ~rules  () = { rules }
    let parse xml =
      Some { rules = (Aws.Xml.required "Rule" (Rules.parse xml)) }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Rule", (Rules.to_query v.rules)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt [Some ("Rule", (Rules.to_json v.rules))])
    let of_json j =
      {
        rules =
          (Rules.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Rule")))
      }
  end
module RequestCharged =
  struct
    type t =
      | Requester 
    let str_to_t = [("requester", Requester)]
    let t_to_str = [(Requester, "requester")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ReplicationStatus =
  struct
    type t =
      | COMPLETE 
      | PENDING 
      | FAILED 
      | REPLICA 
    let str_to_t =
      [("REPLICA", REPLICA);
      ("FAILED", FAILED);
      ("PENDING", PENDING);
      ("COMPLETE", COMPLETE)]
    let t_to_str =
      [(REPLICA, "REPLICA");
      (FAILED, "FAILED");
      (PENDING, "PENDING");
      (COMPLETE, "COMPLETE")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ObjectLockMode =
  struct
    type t =
      | GOVERNANCE 
      | COMPLIANCE 
    let str_to_t = [("COMPLIANCE", COMPLIANCE); ("GOVERNANCE", GOVERNANCE)]
    let t_to_str = [(COMPLIANCE, "COMPLIANCE"); (GOVERNANCE, "GOVERNANCE")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ObjectLockLegalHoldStatus =
  struct
    type t =
      | ON 
      | OFF 
    let str_to_t = [("OFF", OFF); ("ON", ON)]
    let t_to_str = [(OFF, "OFF"); (ON, "ON")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module GetObjectOutput =
  struct
    type t =
      {
      body: Blob.t option ;
      delete_marker: Boolean.t option ;
      accept_ranges: String.t option ;
      expiration: String.t option ;
      restore: String.t option ;
      last_modified: DateTime.t option ;
      content_length: Long.t option ;
      e_tag: String.t option ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option ;
      missing_meta: Integer.t option ;
      version_id: String.t option ;
      cache_control: String.t option ;
      content_disposition: String.t option ;
      content_encoding: String.t option ;
      content_language: String.t option ;
      content_range: String.t option ;
      content_type: String.t option ;
      expires: DateTime.t option ;
      website_redirect_location: String.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      metadata: Metadata.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      bucket_key_enabled: Boolean.t option ;
      storage_class: StorageClass.t option ;
      request_charged: RequestCharged.t option ;
      replication_status: ReplicationStatus.t option ;
      parts_count: Integer.t option ;
      tag_count: Integer.t option ;
      object_lock_mode: ObjectLockMode.t option ;
      object_lock_retain_until_date: DateTime.t option ;
      object_lock_legal_hold_status: ObjectLockLegalHoldStatus.t option }
    let make ?body  ?delete_marker  ?accept_ranges  ?expiration  ?restore 
      ?last_modified  ?content_length  ?e_tag  ?checksum_c_r_c32 
      ?checksum_c_r_c32_c  ?checksum_s_h_a1  ?checksum_s_h_a256 
      ?missing_meta  ?version_id  ?cache_control  ?content_disposition 
      ?content_encoding  ?content_language  ?content_range  ?content_type 
      ?expires  ?website_redirect_location  ?server_side_encryption 
      ?metadata  ?s_s_e_customer_algorithm  ?s_s_e_customer_key_m_d5 
      ?s_s_e_k_m_s_key_id  ?bucket_key_enabled  ?storage_class 
      ?request_charged  ?replication_status  ?parts_count  ?tag_count 
      ?object_lock_mode  ?object_lock_retain_until_date 
      ?object_lock_legal_hold_status  () =
      {
        body;
        delete_marker;
        accept_ranges;
        expiration;
        restore;
        last_modified;
        content_length;
        e_tag;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256;
        missing_meta;
        version_id;
        cache_control;
        content_disposition;
        content_encoding;
        content_language;
        content_range;
        content_type;
        expires;
        website_redirect_location;
        server_side_encryption;
        metadata;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        bucket_key_enabled;
        storage_class;
        request_charged;
        replication_status;
        parts_count;
        tag_count;
        object_lock_mode;
        object_lock_retain_until_date;
        object_lock_legal_hold_status
      }
    let parse xml =
      Some
        {
          body =
            (Aws.Util.option_bind (Aws.Xml.member "Body" xml) Blob.parse);
          delete_marker =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-delete-marker" xml)
               Boolean.parse);
          accept_ranges =
            (Aws.Util.option_bind (Aws.Xml.member "accept-ranges" xml)
               String.parse);
          expiration =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-expiration" xml)
               String.parse);
          restore =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-restore" xml)
               String.parse);
          last_modified =
            (Aws.Util.option_bind (Aws.Xml.member "Last-Modified" xml)
               DateTime.parse);
          content_length =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Length" xml)
               Long.parse);
          e_tag =
            (Aws.Util.option_bind (Aws.Xml.member "ETag" xml) String.parse);
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-crc32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-crc32c" xml) String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-sha1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-sha256" xml) String.parse);
          missing_meta =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-missing-meta" xml)
               Integer.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-version-id" xml)
               String.parse);
          cache_control =
            (Aws.Util.option_bind (Aws.Xml.member "Cache-Control" xml)
               String.parse);
          content_disposition =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Disposition" xml)
               String.parse);
          content_encoding =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Encoding" xml)
               String.parse);
          content_language =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Language" xml)
               String.parse);
          content_range =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Range" xml)
               String.parse);
          content_type =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Type" xml)
               String.parse);
          expires =
            (Aws.Util.option_bind (Aws.Xml.member "Expires" xml)
               DateTime.parse);
          website_redirect_location =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-website-redirect-location" xml)
               String.parse);
          server_side_encryption =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          metadata =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-meta-" xml)
               Metadata.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          s_s_e_k_m_s_key_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-aws-kms-key-id"
                  xml) String.parse);
          bucket_key_enabled =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-bucket-key-enabled" xml)
               Boolean.parse);
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-storage-class" xml)
               StorageClass.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse);
          replication_status =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-replication-status" xml)
               ReplicationStatus.parse);
          parts_count =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-mp-parts-count" xml)
               Integer.parse);
          tag_count =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-tagging-count" xml)
               Integer.parse);
          object_lock_mode =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-mode" xml)
               ObjectLockMode.parse);
          object_lock_retain_until_date =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-retain-until-date" xml)
               DateTime.parse);
          object_lock_legal_hold_status =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-legal-hold" xml)
               ObjectLockLegalHoldStatus.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.object_lock_legal_hold_status
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-object-lock-legal-hold",
                     (ObjectLockLegalHoldStatus.to_query f)));
           Aws.Util.option_map v.object_lock_retain_until_date
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-object-lock-retain-until-date",
                    (DateTime.to_query f)));
           Aws.Util.option_map v.object_lock_mode
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-object-lock-mode", (ObjectLockMode.to_query f)));
           Aws.Util.option_map v.tag_count
             (fun f ->
                Aws.Query.Pair ("x-amz-tagging-count", (Integer.to_query f)));
           Aws.Util.option_map v.parts_count
             (fun f ->
                Aws.Query.Pair ("x-amz-mp-parts-count", (Integer.to_query f)));
           Aws.Util.option_map v.replication_status
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-replication-status",
                    (ReplicationStatus.to_query f)));
           Aws.Util.option_map v.request_charged
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-storage-class", (StorageClass.to_query f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-bucket-key-enabled",
                    (Boolean.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.metadata
             (fun f -> Aws.Query.Pair ("x-amz-meta-", (Metadata.to_query f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Aws.Util.option_map v.website_redirect_location
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-website-redirect-location", (String.to_query f)));
           Aws.Util.option_map v.expires
             (fun f -> Aws.Query.Pair ("Expires", (DateTime.to_query f)));
           Aws.Util.option_map v.content_type
             (fun f -> Aws.Query.Pair ("Content-Type", (String.to_query f)));
           Aws.Util.option_map v.content_range
             (fun f -> Aws.Query.Pair ("Content-Range", (String.to_query f)));
           Aws.Util.option_map v.content_language
             (fun f ->
                Aws.Query.Pair ("Content-Language", (String.to_query f)));
           Aws.Util.option_map v.content_encoding
             (fun f ->
                Aws.Query.Pair ("Content-Encoding", (String.to_query f)));
           Aws.Util.option_map v.content_disposition
             (fun f ->
                Aws.Query.Pair ("Content-Disposition", (String.to_query f)));
           Aws.Util.option_map v.cache_control
             (fun f -> Aws.Query.Pair ("Cache-Control", (String.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f ->
                Aws.Query.Pair ("x-amz-version-id", (String.to_query f)));
           Aws.Util.option_map v.missing_meta
             (fun f ->
                Aws.Query.Pair ("x-amz-missing-meta", (Integer.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32c", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32", (String.to_query f)));
           Aws.Util.option_map v.e_tag
             (fun f -> Aws.Query.Pair ("ETag", (String.to_query f)));
           Aws.Util.option_map v.content_length
             (fun f -> Aws.Query.Pair ("Content-Length", (Long.to_query f)));
           Aws.Util.option_map v.last_modified
             (fun f ->
                Aws.Query.Pair ("Last-Modified", (DateTime.to_query f)));
           Aws.Util.option_map v.restore
             (fun f -> Aws.Query.Pair ("x-amz-restore", (String.to_query f)));
           Aws.Util.option_map v.expiration
             (fun f ->
                Aws.Query.Pair ("x-amz-expiration", (String.to_query f)));
           Aws.Util.option_map v.accept_ranges
             (fun f -> Aws.Query.Pair ("accept-ranges", (String.to_query f)));
           Aws.Util.option_map v.delete_marker
             (fun f ->
                Aws.Query.Pair ("x-amz-delete-marker", (Boolean.to_query f)));
           Aws.Util.option_map v.body
             (fun f -> Aws.Query.Pair ("Body", (Blob.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.object_lock_legal_hold_status
              (fun f ->
                 ("x-amz-object-lock-legal-hold",
                   (ObjectLockLegalHoldStatus.to_json f)));
           Aws.Util.option_map v.object_lock_retain_until_date
             (fun f ->
                ("x-amz-object-lock-retain-until-date", (DateTime.to_json f)));
           Aws.Util.option_map v.object_lock_mode
             (fun f -> ("x-amz-object-lock-mode", (ObjectLockMode.to_json f)));
           Aws.Util.option_map v.tag_count
             (fun f -> ("x-amz-tagging-count", (Integer.to_json f)));
           Aws.Util.option_map v.parts_count
             (fun f -> ("x-amz-mp-parts-count", (Integer.to_json f)));
           Aws.Util.option_map v.replication_status
             (fun f ->
                ("x-amz-replication-status", (ReplicationStatus.to_json f)));
           Aws.Util.option_map v.request_charged
             (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f -> ("x-amz-storage-class", (StorageClass.to_json f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                ("x-amz-server-side-encryption-bucket-key-enabled",
                  (Boolean.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                ("x-amz-server-side-encryption-aws-kms-key-id",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.metadata
             (fun f -> ("x-amz-meta-", (Metadata.to_json f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                ("x-amz-server-side-encryption",
                  (ServerSideEncryption.to_json f)));
           Aws.Util.option_map v.website_redirect_location
             (fun f ->
                ("x-amz-website-redirect-location", (String.to_json f)));
           Aws.Util.option_map v.expires
             (fun f -> ("Expires", (DateTime.to_json f)));
           Aws.Util.option_map v.content_type
             (fun f -> ("Content-Type", (String.to_json f)));
           Aws.Util.option_map v.content_range
             (fun f -> ("Content-Range", (String.to_json f)));
           Aws.Util.option_map v.content_language
             (fun f -> ("Content-Language", (String.to_json f)));
           Aws.Util.option_map v.content_encoding
             (fun f -> ("Content-Encoding", (String.to_json f)));
           Aws.Util.option_map v.content_disposition
             (fun f -> ("Content-Disposition", (String.to_json f)));
           Aws.Util.option_map v.cache_control
             (fun f -> ("Cache-Control", (String.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("x-amz-version-id", (String.to_json f)));
           Aws.Util.option_map v.missing_meta
             (fun f -> ("x-amz-missing-meta", (Integer.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f -> ("x-amz-checksum-sha256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("x-amz-checksum-sha1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("x-amz-checksum-crc32c", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("x-amz-checksum-crc32", (String.to_json f)));
           Aws.Util.option_map v.e_tag
             (fun f -> ("ETag", (String.to_json f)));
           Aws.Util.option_map v.content_length
             (fun f -> ("Content-Length", (Long.to_json f)));
           Aws.Util.option_map v.last_modified
             (fun f -> ("Last-Modified", (DateTime.to_json f)));
           Aws.Util.option_map v.restore
             (fun f -> ("x-amz-restore", (String.to_json f)));
           Aws.Util.option_map v.expiration
             (fun f -> ("x-amz-expiration", (String.to_json f)));
           Aws.Util.option_map v.accept_ranges
             (fun f -> ("accept-ranges", (String.to_json f)));
           Aws.Util.option_map v.delete_marker
             (fun f -> ("x-amz-delete-marker", (Boolean.to_json f)));
           Aws.Util.option_map v.body (fun f -> ("Body", (Blob.to_json f)))])
    let of_json j =
      {
        body = (Aws.Util.option_map (Aws.Json.lookup j "Body") Blob.of_json);
        delete_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-delete-marker")
             Boolean.of_json);
        accept_ranges =
          (Aws.Util.option_map (Aws.Json.lookup j "accept-ranges")
             String.of_json);
        expiration =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-expiration")
             String.of_json);
        restore =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-restore")
             String.of_json);
        last_modified =
          (Aws.Util.option_map (Aws.Json.lookup j "Last-Modified")
             DateTime.of_json);
        content_length =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Length")
             Long.of_json);
        e_tag =
          (Aws.Util.option_map (Aws.Json.lookup j "ETag") String.of_json);
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32c")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha256")
             String.of_json);
        missing_meta =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-missing-meta")
             Integer.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-version-id")
             String.of_json);
        cache_control =
          (Aws.Util.option_map (Aws.Json.lookup j "Cache-Control")
             String.of_json);
        content_disposition =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Disposition")
             String.of_json);
        content_encoding =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Encoding")
             String.of_json);
        content_language =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Language")
             String.of_json);
        content_range =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Range")
             String.of_json);
        content_type =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Type")
             String.of_json);
        expires =
          (Aws.Util.option_map (Aws.Json.lookup j "Expires") DateTime.of_json);
        website_redirect_location =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-website-redirect-location")
             String.of_json);
        server_side_encryption =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption")
             ServerSideEncryption.of_json);
        metadata =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-meta-")
             Metadata.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-aws-kms-key-id")
             String.of_json);
        bucket_key_enabled =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-bucket-key-enabled")
             Boolean.of_json);
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-storage-class")
             StorageClass.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json);
        replication_status =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-replication-status")
             ReplicationStatus.of_json);
        parts_count =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-mp-parts-count")
             Integer.of_json);
        tag_count =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-tagging-count")
             Integer.of_json);
        object_lock_mode =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-object-lock-mode")
             ObjectLockMode.of_json);
        object_lock_retain_until_date =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-object-lock-retain-until-date")
             DateTime.of_json);
        object_lock_legal_hold_status =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-object-lock-legal-hold")
             ObjectLockLegalHoldStatus.of_json)
      }
  end
module InventoryFormat =
  struct
    type t =
      | CSV 
      | ORC 
      | Parquet 
    let str_to_t = [("Parquet", Parquet); ("ORC", ORC); ("CSV", CSV)]
    let t_to_str = [(Parquet, "Parquet"); (ORC, "ORC"); (CSV, "CSV")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module GetBucketNotificationConfigurationRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module RequestPayer =
  struct
    type t =
      | Requester 
    let str_to_t = [("requester", Requester)]
    let t_to_str = [(Requester, "requester")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module EncodingType =
  struct
    type t =
      | Url 
    let str_to_t = [("url", Url)]
    let t_to_str = [(Url, "url")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ListMultipartUploadsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      delimiter: String.t option ;
      encoding_type: EncodingType.t option ;
      key_marker: String.t option ;
      max_uploads: Integer.t option ;
      prefix: String.t option ;
      upload_id_marker: String.t option ;
      expected_bucket_owner: String.t option ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ?delimiter  ?encoding_type  ?key_marker  ?max_uploads 
      ?prefix  ?upload_id_marker  ?expected_bucket_owner  ?request_payer  ()
      =
      {
        bucket;
        delimiter;
        encoding_type;
        key_marker;
        max_uploads;
        prefix;
        upload_id_marker;
        expected_bucket_owner;
        request_payer
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          delimiter =
            (Aws.Util.option_bind (Aws.Xml.member "delimiter" xml)
               String.parse);
          encoding_type =
            (Aws.Util.option_bind (Aws.Xml.member "encoding-type" xml)
               EncodingType.parse);
          key_marker =
            (Aws.Util.option_bind (Aws.Xml.member "key-marker" xml)
               String.parse);
          max_uploads =
            (Aws.Util.option_bind (Aws.Xml.member "max-uploads" xml)
               Integer.parse);
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "prefix" xml) String.parse);
          upload_id_marker =
            (Aws.Util.option_bind (Aws.Xml.member "upload-id-marker" xml)
               String.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_payer
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.upload_id_marker
             (fun f ->
                Aws.Query.Pair ("upload-id-marker", (String.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("prefix", (String.to_query f)));
           Aws.Util.option_map v.max_uploads
             (fun f -> Aws.Query.Pair ("max-uploads", (Integer.to_query f)));
           Aws.Util.option_map v.key_marker
             (fun f -> Aws.Query.Pair ("key-marker", (String.to_query f)));
           Aws.Util.option_map v.encoding_type
             (fun f ->
                Aws.Query.Pair ("encoding-type", (EncodingType.to_query f)));
           Aws.Util.option_map v.delimiter
             (fun f -> Aws.Query.Pair ("delimiter", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_payer
              (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.upload_id_marker
             (fun f -> ("upload-id-marker", (String.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("prefix", (String.to_json f)));
           Aws.Util.option_map v.max_uploads
             (fun f -> ("max-uploads", (Integer.to_json f)));
           Aws.Util.option_map v.key_marker
             (fun f -> ("key-marker", (String.to_json f)));
           Aws.Util.option_map v.encoding_type
             (fun f -> ("encoding-type", (EncodingType.to_json f)));
           Aws.Util.option_map v.delimiter
             (fun f -> ("delimiter", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        delimiter =
          (Aws.Util.option_map (Aws.Json.lookup j "delimiter") String.of_json);
        encoding_type =
          (Aws.Util.option_map (Aws.Json.lookup j "encoding-type")
             EncodingType.of_json);
        key_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "key-marker")
             String.of_json);
        max_uploads =
          (Aws.Util.option_map (Aws.Json.lookup j "max-uploads")
             Integer.of_json);
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "prefix") String.of_json);
        upload_id_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "upload-id-marker")
             String.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json)
      }
  end
module DeleteBucketOwnershipControlsRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module TransitionList =
  struct
    type t = Transition.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map Transition.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list Transition.to_query v
    let to_json v = `List (List.map Transition.to_json v)
    let of_json j = Aws.Json.to_list Transition.of_json j
  end
module NoncurrentVersionTransitionList =
  struct
    type t = NoncurrentVersionTransition.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map NoncurrentVersionTransition.parse
           (Aws.Xml.members "member" xml))
    let to_query v =
      Aws.Query.to_query_list NoncurrentVersionTransition.to_query v
    let to_json v = `List (List.map NoncurrentVersionTransition.to_json v)
    let of_json j = Aws.Json.to_list NoncurrentVersionTransition.of_json j
  end
module LifecycleRuleAndOperator =
  struct
    type t =
      {
      prefix: String.t option ;
      tags: TagSet.t ;
      object_size_greater_than: Long.t option ;
      object_size_less_than: Long.t option }
    let make ?prefix  ?(tags= [])  ?object_size_greater_than 
      ?object_size_less_than  () =
      { prefix; tags; object_size_greater_than; object_size_less_than }
    let parse xml =
      Some
        {
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          tags =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "Tag" xml) TagSet.parse));
          object_size_greater_than =
            (Aws.Util.option_bind
               (Aws.Xml.member "ObjectSizeGreaterThan" xml) Long.parse);
          object_size_less_than =
            (Aws.Util.option_bind (Aws.Xml.member "ObjectSizeLessThan" xml)
               Long.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.object_size_less_than
              (fun f ->
                 Aws.Query.Pair ("ObjectSizeLessThan", (Long.to_query f)));
           Aws.Util.option_map v.object_size_greater_than
             (fun f ->
                Aws.Query.Pair ("ObjectSizeGreaterThan", (Long.to_query f)));
           Some (Aws.Query.Pair ("Tag", (TagSet.to_query v.tags)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.object_size_less_than
              (fun f -> ("ObjectSizeLessThan", (Long.to_json f)));
           Aws.Util.option_map v.object_size_greater_than
             (fun f -> ("ObjectSizeGreaterThan", (Long.to_json f)));
           Some ("Tag", (TagSet.to_json v.tags));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)))])
    let of_json j =
      {
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        tags =
          (TagSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tag")));
        object_size_greater_than =
          (Aws.Util.option_map (Aws.Json.lookup j "ObjectSizeGreaterThan")
             Long.of_json);
        object_size_less_than =
          (Aws.Util.option_map (Aws.Json.lookup j "ObjectSizeLessThan")
             Long.of_json)
      }
  end
module LifecycleRuleFilter =
  struct
    type t =
      {
      prefix: String.t option ;
      tag: Tag.t option ;
      object_size_greater_than: Long.t option ;
      object_size_less_than: Long.t option ;
      and_: LifecycleRuleAndOperator.t option }
    let make ?prefix  ?tag  ?object_size_greater_than  ?object_size_less_than
       ?and_  () =
      { prefix; tag; object_size_greater_than; object_size_less_than; and_ }
    let parse xml =
      Some
        {
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          tag = (Aws.Util.option_bind (Aws.Xml.member "Tag" xml) Tag.parse);
          object_size_greater_than =
            (Aws.Util.option_bind
               (Aws.Xml.member "ObjectSizeGreaterThan" xml) Long.parse);
          object_size_less_than =
            (Aws.Util.option_bind (Aws.Xml.member "ObjectSizeLessThan" xml)
               Long.parse);
          and_ =
            (Aws.Util.option_bind (Aws.Xml.member "And" xml)
               LifecycleRuleAndOperator.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.and_
              (fun f ->
                 Aws.Query.Pair
                   ("And", (LifecycleRuleAndOperator.to_query f)));
           Aws.Util.option_map v.object_size_less_than
             (fun f ->
                Aws.Query.Pair ("ObjectSizeLessThan", (Long.to_query f)));
           Aws.Util.option_map v.object_size_greater_than
             (fun f ->
                Aws.Query.Pair ("ObjectSizeGreaterThan", (Long.to_query f)));
           Aws.Util.option_map v.tag
             (fun f -> Aws.Query.Pair ("Tag", (Tag.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.and_
              (fun f -> ("And", (LifecycleRuleAndOperator.to_json f)));
           Aws.Util.option_map v.object_size_less_than
             (fun f -> ("ObjectSizeLessThan", (Long.to_json f)));
           Aws.Util.option_map v.object_size_greater_than
             (fun f -> ("ObjectSizeGreaterThan", (Long.to_json f)));
           Aws.Util.option_map v.tag (fun f -> ("Tag", (Tag.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)))])
    let of_json j =
      {
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        tag = (Aws.Util.option_map (Aws.Json.lookup j "Tag") Tag.of_json);
        object_size_greater_than =
          (Aws.Util.option_map (Aws.Json.lookup j "ObjectSizeGreaterThan")
             Long.of_json);
        object_size_less_than =
          (Aws.Util.option_map (Aws.Json.lookup j "ObjectSizeLessThan")
             Long.of_json);
        and_ =
          (Aws.Util.option_map (Aws.Json.lookup j "And")
             LifecycleRuleAndOperator.of_json)
      }
  end
module LifecycleRule =
  struct
    type t =
      {
      expiration: LifecycleExpiration.t option ;
      i_d: String.t option ;
      prefix: String.t option ;
      filter: LifecycleRuleFilter.t option ;
      status: ExpirationStatus.t ;
      transitions: TransitionList.t ;
      noncurrent_version_transitions: NoncurrentVersionTransitionList.t ;
      noncurrent_version_expiration: NoncurrentVersionExpiration.t option ;
      abort_incomplete_multipart_upload:
        AbortIncompleteMultipartUpload.t option }
    let make ?expiration  ?i_d  ?prefix  ?filter  ~status  ?(transitions= [])
       ?(noncurrent_version_transitions= [])  ?noncurrent_version_expiration 
      ?abort_incomplete_multipart_upload  () =
      {
        expiration;
        i_d;
        prefix;
        filter;
        status;
        transitions;
        noncurrent_version_transitions;
        noncurrent_version_expiration;
        abort_incomplete_multipart_upload
      }
    let parse xml =
      Some
        {
          expiration =
            (Aws.Util.option_bind (Aws.Xml.member "Expiration" xml)
               LifecycleExpiration.parse);
          i_d = (Aws.Util.option_bind (Aws.Xml.member "ID" xml) String.parse);
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          filter =
            (Aws.Util.option_bind (Aws.Xml.member "Filter" xml)
               LifecycleRuleFilter.parse);
          status =
            (Aws.Xml.required "Status"
               (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
                  ExpirationStatus.parse));
          transitions = (Aws.Util.of_option [] (TransitionList.parse xml));
          noncurrent_version_transitions =
            (Aws.Util.of_option []
               (NoncurrentVersionTransitionList.parse xml));
          noncurrent_version_expiration =
            (Aws.Util.option_bind
               (Aws.Xml.member "NoncurrentVersionExpiration" xml)
               NoncurrentVersionExpiration.parse);
          abort_incomplete_multipart_upload =
            (Aws.Util.option_bind
               (Aws.Xml.member "AbortIncompleteMultipartUpload" xml)
               AbortIncompleteMultipartUpload.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.abort_incomplete_multipart_upload
              (fun f ->
                 Aws.Query.Pair
                   ("AbortIncompleteMultipartUpload",
                     (AbortIncompleteMultipartUpload.to_query f)));
           Aws.Util.option_map v.noncurrent_version_expiration
             (fun f ->
                Aws.Query.Pair
                  ("NoncurrentVersionExpiration",
                    (NoncurrentVersionExpiration.to_query f)));
           Some
             (Aws.Query.Pair
                ("NoncurrentVersionTransition",
                  (NoncurrentVersionTransitionList.to_query
                     v.noncurrent_version_transitions)));
           Some
             (Aws.Query.Pair
                ("Transition", (TransitionList.to_query v.transitions)));
           Some
             (Aws.Query.Pair ("Status", (ExpirationStatus.to_query v.status)));
           Aws.Util.option_map v.filter
             (fun f ->
                Aws.Query.Pair ("Filter", (LifecycleRuleFilter.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)));
           Aws.Util.option_map v.i_d
             (fun f -> Aws.Query.Pair ("ID", (String.to_query f)));
           Aws.Util.option_map v.expiration
             (fun f ->
                Aws.Query.Pair
                  ("Expiration", (LifecycleExpiration.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.abort_incomplete_multipart_upload
              (fun f ->
                 ("AbortIncompleteMultipartUpload",
                   (AbortIncompleteMultipartUpload.to_json f)));
           Aws.Util.option_map v.noncurrent_version_expiration
             (fun f ->
                ("NoncurrentVersionExpiration",
                  (NoncurrentVersionExpiration.to_json f)));
           Some
             ("NoncurrentVersionTransition",
               (NoncurrentVersionTransitionList.to_json
                  v.noncurrent_version_transitions));
           Some ("Transition", (TransitionList.to_json v.transitions));
           Some ("Status", (ExpirationStatus.to_json v.status));
           Aws.Util.option_map v.filter
             (fun f -> ("Filter", (LifecycleRuleFilter.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)));
           Aws.Util.option_map v.i_d (fun f -> ("ID", (String.to_json f)));
           Aws.Util.option_map v.expiration
             (fun f -> ("Expiration", (LifecycleExpiration.to_json f)))])
    let of_json j =
      {
        expiration =
          (Aws.Util.option_map (Aws.Json.lookup j "Expiration")
             LifecycleExpiration.of_json);
        i_d = (Aws.Util.option_map (Aws.Json.lookup j "ID") String.of_json);
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        filter =
          (Aws.Util.option_map (Aws.Json.lookup j "Filter")
             LifecycleRuleFilter.of_json);
        status =
          (ExpirationStatus.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Status")));
        transitions =
          (TransitionList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Transition")));
        noncurrent_version_transitions =
          (NoncurrentVersionTransitionList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "NoncurrentVersionTransition")));
        noncurrent_version_expiration =
          (Aws.Util.option_map
             (Aws.Json.lookup j "NoncurrentVersionExpiration")
             NoncurrentVersionExpiration.of_json);
        abort_incomplete_multipart_upload =
          (Aws.Util.option_map
             (Aws.Json.lookup j "AbortIncompleteMultipartUpload")
             AbortIncompleteMultipartUpload.of_json)
      }
  end
module LifecycleRules =
  struct
    type t = LifecycleRule.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map LifecycleRule.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list LifecycleRule.to_query v
    let to_json v = `List (List.map LifecycleRule.to_json v)
    let of_json j = Aws.Json.to_list LifecycleRule.of_json j
  end
module BucketLifecycleConfiguration =
  struct
    type t = {
      rules: LifecycleRules.t }
    let make ~rules  () = { rules }
    let parse xml =
      Some { rules = (Aws.Xml.required "Rule" (LifecycleRules.parse xml)) }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Rule", (LifecycleRules.to_query v.rules)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Rule", (LifecycleRules.to_json v.rules))])
    let of_json j =
      {
        rules =
          (LifecycleRules.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Rule")))
      }
  end
module ObjectLockRetentionMode =
  struct
    type t =
      | GOVERNANCE 
      | COMPLIANCE 
    let str_to_t = [("COMPLIANCE", COMPLIANCE); ("GOVERNANCE", GOVERNANCE)]
    let t_to_str = [(COMPLIANCE, "COMPLIANCE"); (GOVERNANCE, "GOVERNANCE")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ObjectLockRetention =
  struct
    type t =
      {
      mode: ObjectLockRetentionMode.t option ;
      retain_until_date: DateTime.t option }
    let make ?mode  ?retain_until_date  () = { mode; retain_until_date }
    let parse xml =
      Some
        {
          mode =
            (Aws.Util.option_bind (Aws.Xml.member "Mode" xml)
               ObjectLockRetentionMode.parse);
          retain_until_date =
            (Aws.Util.option_bind (Aws.Xml.member "RetainUntilDate" xml)
               DateTime.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.retain_until_date
              (fun f ->
                 Aws.Query.Pair ("RetainUntilDate", (DateTime.to_query f)));
           Aws.Util.option_map v.mode
             (fun f ->
                Aws.Query.Pair ("Mode", (ObjectLockRetentionMode.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.retain_until_date
              (fun f -> ("RetainUntilDate", (DateTime.to_json f)));
           Aws.Util.option_map v.mode
             (fun f -> ("Mode", (ObjectLockRetentionMode.to_json f)))])
    let of_json j =
      {
        mode =
          (Aws.Util.option_map (Aws.Json.lookup j "Mode")
             ObjectLockRetentionMode.of_json);
        retain_until_date =
          (Aws.Util.option_map (Aws.Json.lookup j "RetainUntilDate")
             DateTime.of_json)
      }
  end
module ChecksumAlgorithm =
  struct
    type t =
      | CRC32 
      | CRC32C 
      | SHA1 
      | SHA256 
    let str_to_t =
      [("SHA256", SHA256);
      ("SHA1", SHA1);
      ("CRC32C", CRC32C);
      ("CRC32", CRC32)]
    let t_to_str =
      [(SHA256, "SHA256");
      (SHA1, "SHA1");
      (CRC32C, "CRC32C");
      (CRC32, "CRC32")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module PutObjectRetentionRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      retention: ObjectLockRetention.t option ;
      request_payer: RequestPayer.t option ;
      version_id: String.t option ;
      bypass_governance_retention: Boolean.t option ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~key  ?retention  ?request_payer  ?version_id 
      ?bypass_governance_retention  ?content_m_d5  ?checksum_algorithm 
      ?expected_bucket_owner  () =
      {
        bucket;
        key;
        retention;
        request_payer;
        version_id;
        bypass_governance_retention;
        content_m_d5;
        checksum_algorithm;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          retention =
            (Aws.Util.option_bind (Aws.Xml.member "Retention" xml)
               ObjectLockRetention.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          bypass_governance_retention =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-bypass-governance-retention" xml)
               Boolean.parse);
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Aws.Util.option_map v.bypass_governance_retention
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-bypass-governance-retention", (Boolean.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.retention
             (fun f ->
                Aws.Query.Pair
                  ("Retention", (ObjectLockRetention.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Aws.Util.option_map v.bypass_governance_retention
             (fun f ->
                ("x-amz-bypass-governance-retention", (Boolean.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.retention
             (fun f -> ("Retention", (ObjectLockRetention.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        retention =
          (Aws.Util.option_map (Aws.Json.lookup j "Retention")
             ObjectLockRetention.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        bypass_governance_retention =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-bypass-governance-retention")
             Boolean.of_json);
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module PutBucketTaggingRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      tagging: Tagging.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?content_m_d5  ?checksum_algorithm  ~tagging 
      ?expected_bucket_owner  () =
      {
        bucket;
        content_m_d5;
        checksum_algorithm;
        tagging;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          tagging =
            (Aws.Xml.required "Tagging"
               (Aws.Util.option_bind (Aws.Xml.member "Tagging" xml)
                  Tagging.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Tagging", (Tagging.to_query v.tagging)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Tagging", (Tagging.to_json v.tagging));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        tagging =
          (Tagging.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Tagging")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module InventoryFrequency =
  struct
    type t =
      | Daily 
      | Weekly 
    let str_to_t = [("Weekly", Weekly); ("Daily", Daily)]
    let t_to_str = [(Weekly, "Weekly"); (Daily, "Daily")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module InventorySchedule =
  struct
    type t = {
      frequency: InventoryFrequency.t }
    let make ~frequency  () = { frequency }
    let parse xml =
      Some
        {
          frequency =
            (Aws.Xml.required "Frequency"
               (Aws.Util.option_bind (Aws.Xml.member "Frequency" xml)
                  InventoryFrequency.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("Frequency", (InventoryFrequency.to_query v.frequency)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Frequency", (InventoryFrequency.to_json v.frequency))])
    let of_json j =
      {
        frequency =
          (InventoryFrequency.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Frequency")))
      }
  end
module InventoryOptionalField =
  struct
    type t =
      | Size 
      | LastModifiedDate 
      | StorageClass 
      | ETag 
      | IsMultipartUploaded 
      | ReplicationStatus 
      | EncryptionStatus 
      | ObjectLockRetainUntilDate 
      | ObjectLockMode 
      | ObjectLockLegalHoldStatus 
      | IntelligentTieringAccessTier 
      | BucketKeyStatus 
      | ChecksumAlgorithm 
    let str_to_t =
      [("ChecksumAlgorithm", ChecksumAlgorithm);
      ("BucketKeyStatus", BucketKeyStatus);
      ("IntelligentTieringAccessTier", IntelligentTieringAccessTier);
      ("ObjectLockLegalHoldStatus", ObjectLockLegalHoldStatus);
      ("ObjectLockMode", ObjectLockMode);
      ("ObjectLockRetainUntilDate", ObjectLockRetainUntilDate);
      ("EncryptionStatus", EncryptionStatus);
      ("ReplicationStatus", ReplicationStatus);
      ("IsMultipartUploaded", IsMultipartUploaded);
      ("ETag", ETag);
      ("StorageClass", StorageClass);
      ("LastModifiedDate", LastModifiedDate);
      ("Size", Size)]
    let t_to_str =
      [(ChecksumAlgorithm, "ChecksumAlgorithm");
      (BucketKeyStatus, "BucketKeyStatus");
      (IntelligentTieringAccessTier, "IntelligentTieringAccessTier");
      (ObjectLockLegalHoldStatus, "ObjectLockLegalHoldStatus");
      (ObjectLockMode, "ObjectLockMode");
      (ObjectLockRetainUntilDate, "ObjectLockRetainUntilDate");
      (EncryptionStatus, "EncryptionStatus");
      (ReplicationStatus, "ReplicationStatus");
      (IsMultipartUploaded, "IsMultipartUploaded");
      (ETag, "ETag");
      (StorageClass, "StorageClass");
      (LastModifiedDate, "LastModifiedDate");
      (Size, "Size")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module InventoryOptionalFields =
  struct
    type t = InventoryOptionalField.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map InventoryOptionalField.parse (Aws.Xml.members "Field" xml))
    let to_query v =
      Aws.Query.to_query_list InventoryOptionalField.to_query v
    let to_json v = `List (List.map InventoryOptionalField.to_json v)
    let of_json j = Aws.Json.to_list InventoryOptionalField.of_json j
  end
module InventoryIncludedObjectVersions =
  struct
    type t =
      | All 
      | Current 
    let str_to_t = [("Current", Current); ("All", All)]
    let t_to_str = [(Current, "Current"); (All, "All")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module InventoryFilter =
  struct
    type t = {
      prefix: String.t }
    let make ~prefix  () = { prefix }
    let parse xml =
      Some
        {
          prefix =
            (Aws.Xml.required "Prefix"
               (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml)
                  String.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Prefix", (String.to_query v.prefix)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Prefix", (String.to_json v.prefix))])
    let of_json j =
      {
        prefix =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Prefix")))
      }
  end
module InventoryS3BucketDestination =
  struct
    type t =
      {
      account_id: String.t option ;
      bucket: String.t ;
      format: InventoryFormat.t ;
      prefix: String.t option ;
      encryption: InventoryEncryption.t option }
    let make ?account_id  ~bucket  ~format  ?prefix  ?encryption  () =
      { account_id; bucket; format; prefix; encryption }
    let parse xml =
      Some
        {
          account_id =
            (Aws.Util.option_bind (Aws.Xml.member "AccountId" xml)
               String.parse);
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          format =
            (Aws.Xml.required "Format"
               (Aws.Util.option_bind (Aws.Xml.member "Format" xml)
                  InventoryFormat.parse));
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          encryption =
            (Aws.Util.option_bind (Aws.Xml.member "Encryption" xml)
               InventoryEncryption.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.encryption
              (fun f ->
                 Aws.Query.Pair
                   ("Encryption", (InventoryEncryption.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)));
           Some
             (Aws.Query.Pair ("Format", (InventoryFormat.to_query v.format)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)));
           Aws.Util.option_map v.account_id
             (fun f -> Aws.Query.Pair ("AccountId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.encryption
              (fun f -> ("Encryption", (InventoryEncryption.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)));
           Some ("Format", (InventoryFormat.to_json v.format));
           Some ("Bucket", (String.to_json v.bucket));
           Aws.Util.option_map v.account_id
             (fun f -> ("AccountId", (String.to_json f)))])
    let of_json j =
      {
        account_id =
          (Aws.Util.option_map (Aws.Json.lookup j "AccountId") String.of_json);
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        format =
          (InventoryFormat.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Format")));
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        encryption =
          (Aws.Util.option_map (Aws.Json.lookup j "Encryption")
             InventoryEncryption.of_json)
      }
  end
module InventoryDestination =
  struct
    type t = {
      s3_bucket_destination: InventoryS3BucketDestination.t }
    let make ~s3_bucket_destination  () = { s3_bucket_destination }
    let parse xml =
      Some
        {
          s3_bucket_destination =
            (Aws.Xml.required "S3BucketDestination"
               (Aws.Util.option_bind
                  (Aws.Xml.member "S3BucketDestination" xml)
                  InventoryS3BucketDestination.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("S3BucketDestination",
                   (InventoryS3BucketDestination.to_query
                      v.s3_bucket_destination)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("S3BucketDestination",
                (InventoryS3BucketDestination.to_json v.s3_bucket_destination))])
    let of_json j =
      {
        s3_bucket_destination =
          (InventoryS3BucketDestination.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "S3BucketDestination")))
      }
  end
module InventoryConfiguration =
  struct
    type t =
      {
      destination: InventoryDestination.t ;
      is_enabled: Boolean.t ;
      filter: InventoryFilter.t option ;
      id: String.t ;
      included_object_versions: InventoryIncludedObjectVersions.t ;
      optional_fields: InventoryOptionalFields.t ;
      schedule: InventorySchedule.t }
    let make ~destination  ~is_enabled  ?filter  ~id 
      ~included_object_versions  ?(optional_fields= [])  ~schedule  () =
      {
        destination;
        is_enabled;
        filter;
        id;
        included_object_versions;
        optional_fields;
        schedule
      }
    let parse xml =
      Some
        {
          destination =
            (Aws.Xml.required "Destination"
               (Aws.Util.option_bind (Aws.Xml.member "Destination" xml)
                  InventoryDestination.parse));
          is_enabled =
            (Aws.Xml.required "IsEnabled"
               (Aws.Util.option_bind (Aws.Xml.member "IsEnabled" xml)
                  Boolean.parse));
          filter =
            (Aws.Util.option_bind (Aws.Xml.member "Filter" xml)
               InventoryFilter.parse);
          id =
            (Aws.Xml.required "Id"
               (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse));
          included_object_versions =
            (Aws.Xml.required "IncludedObjectVersions"
               (Aws.Util.option_bind
                  (Aws.Xml.member "IncludedObjectVersions" xml)
                  InventoryIncludedObjectVersions.parse));
          optional_fields =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "OptionalFields" xml)
                  InventoryOptionalFields.parse));
          schedule =
            (Aws.Xml.required "Schedule"
               (Aws.Util.option_bind (Aws.Xml.member "Schedule" xml)
                  InventorySchedule.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("Schedule", (InventorySchedule.to_query v.schedule)));
           Some
             (Aws.Query.Pair
                ("OptionalFields.member",
                  (InventoryOptionalFields.to_query v.optional_fields)));
           Some
             (Aws.Query.Pair
                ("IncludedObjectVersions",
                  (InventoryIncludedObjectVersions.to_query
                     v.included_object_versions)));
           Some (Aws.Query.Pair ("Id", (String.to_query v.id)));
           Aws.Util.option_map v.filter
             (fun f ->
                Aws.Query.Pair ("Filter", (InventoryFilter.to_query f)));
           Some
             (Aws.Query.Pair ("IsEnabled", (Boolean.to_query v.is_enabled)));
           Some
             (Aws.Query.Pair
                ("Destination",
                  (InventoryDestination.to_query v.destination)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Schedule", (InventorySchedule.to_json v.schedule));
           Some
             ("OptionalFields",
               (InventoryOptionalFields.to_json v.optional_fields));
           Some
             ("IncludedObjectVersions",
               (InventoryIncludedObjectVersions.to_json
                  v.included_object_versions));
           Some ("Id", (String.to_json v.id));
           Aws.Util.option_map v.filter
             (fun f -> ("Filter", (InventoryFilter.to_json f)));
           Some ("IsEnabled", (Boolean.to_json v.is_enabled));
           Some ("Destination", (InventoryDestination.to_json v.destination))])
    let of_json j =
      {
        destination =
          (InventoryDestination.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Destination")));
        is_enabled =
          (Boolean.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "IsEnabled")));
        filter =
          (Aws.Util.option_map (Aws.Json.lookup j "Filter")
             InventoryFilter.of_json);
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")));
        included_object_versions =
          (InventoryIncludedObjectVersions.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "IncludedObjectVersions")));
        optional_fields =
          (InventoryOptionalFields.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionalFields")));
        schedule =
          (InventorySchedule.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Schedule")))
      }
  end
module GetBucketInventoryConfigurationOutput =
  struct
    type t = {
      inventory_configuration: InventoryConfiguration.t option }
    let make ?inventory_configuration  () = { inventory_configuration }
    let parse xml =
      Some
        {
          inventory_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "InventoryConfiguration" xml)
               InventoryConfiguration.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.inventory_configuration
              (fun f ->
                 Aws.Query.Pair
                   ("InventoryConfiguration",
                     (InventoryConfiguration.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.inventory_configuration
              (fun f ->
                 ("InventoryConfiguration",
                   (InventoryConfiguration.to_json f)))])
    let of_json j =
      {
        inventory_configuration =
          (Aws.Util.option_map (Aws.Json.lookup j "InventoryConfiguration")
             InventoryConfiguration.of_json)
      }
  end
module CopyObjectResult =
  struct
    type t =
      {
      e_tag: String.t option ;
      last_modified: DateTime.t option ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option }
    let make ?e_tag  ?last_modified  ?checksum_c_r_c32  ?checksum_c_r_c32_c 
      ?checksum_s_h_a1  ?checksum_s_h_a256  () =
      {
        e_tag;
        last_modified;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256
      }
    let parse xml =
      Some
        {
          e_tag =
            (Aws.Util.option_bind (Aws.Xml.member "ETag" xml) String.parse);
          last_modified =
            (Aws.Util.option_bind (Aws.Xml.member "LastModified" xml)
               DateTime.parse);
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32C" xml)
               String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA256" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_s_h_a256
              (fun f ->
                 Aws.Query.Pair ("ChecksumSHA256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> Aws.Query.Pair ("ChecksumSHA1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> Aws.Query.Pair ("ChecksumCRC32C", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> Aws.Query.Pair ("ChecksumCRC32", (String.to_query f)));
           Aws.Util.option_map v.last_modified
             (fun f -> Aws.Query.Pair ("LastModified", (DateTime.to_query f)));
           Aws.Util.option_map v.e_tag
             (fun f -> Aws.Query.Pair ("ETag", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_s_h_a256
              (fun f -> ("ChecksumSHA256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("ChecksumSHA1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("ChecksumCRC32C", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("ChecksumCRC32", (String.to_json f)));
           Aws.Util.option_map v.last_modified
             (fun f -> ("LastModified", (DateTime.to_json f)));
           Aws.Util.option_map v.e_tag
             (fun f -> ("ETag", (String.to_json f)))])
    let of_json j =
      {
        e_tag =
          (Aws.Util.option_map (Aws.Json.lookup j "ETag") String.of_json);
        last_modified =
          (Aws.Util.option_map (Aws.Json.lookup j "LastModified")
             DateTime.of_json);
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32C")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA256")
             String.of_json)
      }
  end
module CopyObjectOutput =
  struct
    type t =
      {
      copy_object_result: CopyObjectResult.t option ;
      expiration: String.t option ;
      copy_source_version_id: String.t option ;
      version_id: String.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      s_s_e_k_m_s_encryption_context: String.t option ;
      bucket_key_enabled: Boolean.t option ;
      request_charged: RequestCharged.t option }
    let make ?copy_object_result  ?expiration  ?copy_source_version_id 
      ?version_id  ?server_side_encryption  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key_m_d5  ?s_s_e_k_m_s_key_id 
      ?s_s_e_k_m_s_encryption_context  ?bucket_key_enabled  ?request_charged 
      () =
      {
        copy_object_result;
        expiration;
        copy_source_version_id;
        version_id;
        server_side_encryption;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        s_s_e_k_m_s_encryption_context;
        bucket_key_enabled;
        request_charged
      }
    let parse xml =
      Some
        {
          copy_object_result =
            (Aws.Util.option_bind (Aws.Xml.member "CopyObjectResult" xml)
               CopyObjectResult.parse);
          expiration =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-expiration" xml)
               String.parse);
          copy_source_version_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-copy-source-version-id" xml)
               String.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-version-id" xml)
               String.parse);
          server_side_encryption =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          s_s_e_k_m_s_key_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-aws-kms-key-id"
                  xml) String.parse);
          s_s_e_k_m_s_encryption_context =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-context" xml)
               String.parse);
          bucket_key_enabled =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-bucket-key-enabled" xml)
               Boolean.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-bucket-key-enabled",
                    (Boolean.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_encryption_context
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-context",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f ->
                Aws.Query.Pair ("x-amz-version-id", (String.to_query f)));
           Aws.Util.option_map v.copy_source_version_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-version-id", (String.to_query f)));
           Aws.Util.option_map v.expiration
             (fun f ->
                Aws.Query.Pair ("x-amz-expiration", (String.to_query f)));
           Aws.Util.option_map v.copy_object_result
             (fun f ->
                Aws.Query.Pair
                  ("CopyObjectResult", (CopyObjectResult.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                ("x-amz-server-side-encryption-bucket-key-enabled",
                  (Boolean.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_encryption_context
             (fun f ->
                ("x-amz-server-side-encryption-context", (String.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                ("x-amz-server-side-encryption-aws-kms-key-id",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                ("x-amz-server-side-encryption",
                  (ServerSideEncryption.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("x-amz-version-id", (String.to_json f)));
           Aws.Util.option_map v.copy_source_version_id
             (fun f -> ("x-amz-copy-source-version-id", (String.to_json f)));
           Aws.Util.option_map v.expiration
             (fun f -> ("x-amz-expiration", (String.to_json f)));
           Aws.Util.option_map v.copy_object_result
             (fun f -> ("CopyObjectResult", (CopyObjectResult.to_json f)))])
    let of_json j =
      {
        copy_object_result =
          (Aws.Util.option_map (Aws.Json.lookup j "CopyObjectResult")
             CopyObjectResult.of_json);
        expiration =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-expiration")
             String.of_json);
        copy_source_version_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-copy-source-version-id")
             String.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-version-id")
             String.of_json);
        server_side_encryption =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption")
             ServerSideEncryption.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-aws-kms-key-id")
             String.of_json);
        s_s_e_k_m_s_encryption_context =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-context")
             String.of_json);
        bucket_key_enabled =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-bucket-key-enabled")
             Boolean.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module DeleteBucketCorsRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module Initiator =
  struct
    type t = {
      i_d: String.t option ;
      display_name: String.t option }
    let make ?i_d  ?display_name  () = { i_d; display_name }
    let parse xml =
      Some
        {
          i_d = (Aws.Util.option_bind (Aws.Xml.member "ID" xml) String.parse);
          display_name =
            (Aws.Util.option_bind (Aws.Xml.member "DisplayName" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.display_name
              (fun f -> Aws.Query.Pair ("DisplayName", (String.to_query f)));
           Aws.Util.option_map v.i_d
             (fun f -> Aws.Query.Pair ("ID", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.display_name
              (fun f -> ("DisplayName", (String.to_json f)));
           Aws.Util.option_map v.i_d (fun f -> ("ID", (String.to_json f)))])
    let of_json j =
      {
        i_d = (Aws.Util.option_map (Aws.Json.lookup j "ID") String.of_json);
        display_name =
          (Aws.Util.option_map (Aws.Json.lookup j "DisplayName")
             String.of_json)
      }
  end
module GetBucketEncryptionRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module Protocol =
  struct
    type t =
      | Http 
      | Https 
    let str_to_t = [("https", Https); ("http", Http)]
    let t_to_str = [(Https, "https"); (Http, "http")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module Redirect =
  struct
    type t =
      {
      host_name: String.t option ;
      http_redirect_code: String.t option ;
      protocol: Protocol.t option ;
      replace_key_prefix_with: String.t option ;
      replace_key_with: String.t option }
    let make ?host_name  ?http_redirect_code  ?protocol 
      ?replace_key_prefix_with  ?replace_key_with  () =
      {
        host_name;
        http_redirect_code;
        protocol;
        replace_key_prefix_with;
        replace_key_with
      }
    let parse xml =
      Some
        {
          host_name =
            (Aws.Util.option_bind (Aws.Xml.member "HostName" xml)
               String.parse);
          http_redirect_code =
            (Aws.Util.option_bind (Aws.Xml.member "HttpRedirectCode" xml)
               String.parse);
          protocol =
            (Aws.Util.option_bind (Aws.Xml.member "Protocol" xml)
               Protocol.parse);
          replace_key_prefix_with =
            (Aws.Util.option_bind (Aws.Xml.member "ReplaceKeyPrefixWith" xml)
               String.parse);
          replace_key_with =
            (Aws.Util.option_bind (Aws.Xml.member "ReplaceKeyWith" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.replace_key_with
              (fun f ->
                 Aws.Query.Pair ("ReplaceKeyWith", (String.to_query f)));
           Aws.Util.option_map v.replace_key_prefix_with
             (fun f ->
                Aws.Query.Pair ("ReplaceKeyPrefixWith", (String.to_query f)));
           Aws.Util.option_map v.protocol
             (fun f -> Aws.Query.Pair ("Protocol", (Protocol.to_query f)));
           Aws.Util.option_map v.http_redirect_code
             (fun f ->
                Aws.Query.Pair ("HttpRedirectCode", (String.to_query f)));
           Aws.Util.option_map v.host_name
             (fun f -> Aws.Query.Pair ("HostName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.replace_key_with
              (fun f -> ("ReplaceKeyWith", (String.to_json f)));
           Aws.Util.option_map v.replace_key_prefix_with
             (fun f -> ("ReplaceKeyPrefixWith", (String.to_json f)));
           Aws.Util.option_map v.protocol
             (fun f -> ("Protocol", (Protocol.to_json f)));
           Aws.Util.option_map v.http_redirect_code
             (fun f -> ("HttpRedirectCode", (String.to_json f)));
           Aws.Util.option_map v.host_name
             (fun f -> ("HostName", (String.to_json f)))])
    let of_json j =
      {
        host_name =
          (Aws.Util.option_map (Aws.Json.lookup j "HostName") String.of_json);
        http_redirect_code =
          (Aws.Util.option_map (Aws.Json.lookup j "HttpRedirectCode")
             String.of_json);
        protocol =
          (Aws.Util.option_map (Aws.Json.lookup j "Protocol")
             Protocol.of_json);
        replace_key_prefix_with =
          (Aws.Util.option_map (Aws.Json.lookup j "ReplaceKeyPrefixWith")
             String.of_json);
        replace_key_with =
          (Aws.Util.option_map (Aws.Json.lookup j "ReplaceKeyWith")
             String.of_json)
      }
  end
module Condition =
  struct
    type t =
      {
      http_error_code_returned_equals: String.t option ;
      key_prefix_equals: String.t option }
    let make ?http_error_code_returned_equals  ?key_prefix_equals  () =
      { http_error_code_returned_equals; key_prefix_equals }
    let parse xml =
      Some
        {
          http_error_code_returned_equals =
            (Aws.Util.option_bind
               (Aws.Xml.member "HttpErrorCodeReturnedEquals" xml)
               String.parse);
          key_prefix_equals =
            (Aws.Util.option_bind (Aws.Xml.member "KeyPrefixEquals" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.key_prefix_equals
              (fun f ->
                 Aws.Query.Pair ("KeyPrefixEquals", (String.to_query f)));
           Aws.Util.option_map v.http_error_code_returned_equals
             (fun f ->
                Aws.Query.Pair
                  ("HttpErrorCodeReturnedEquals", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.key_prefix_equals
              (fun f -> ("KeyPrefixEquals", (String.to_json f)));
           Aws.Util.option_map v.http_error_code_returned_equals
             (fun f -> ("HttpErrorCodeReturnedEquals", (String.to_json f)))])
    let of_json j =
      {
        http_error_code_returned_equals =
          (Aws.Util.option_map
             (Aws.Json.lookup j "HttpErrorCodeReturnedEquals") String.of_json);
        key_prefix_equals =
          (Aws.Util.option_map (Aws.Json.lookup j "KeyPrefixEquals")
             String.of_json)
      }
  end
module RoutingRule =
  struct
    type t = {
      condition: Condition.t option ;
      redirect: Redirect.t }
    let make ?condition  ~redirect  () = { condition; redirect }
    let parse xml =
      Some
        {
          condition =
            (Aws.Util.option_bind (Aws.Xml.member "Condition" xml)
               Condition.parse);
          redirect =
            (Aws.Xml.required "Redirect"
               (Aws.Util.option_bind (Aws.Xml.member "Redirect" xml)
                  Redirect.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair ("Redirect", (Redirect.to_query v.redirect)));
           Aws.Util.option_map v.condition
             (fun f -> Aws.Query.Pair ("Condition", (Condition.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Redirect", (Redirect.to_json v.redirect));
           Aws.Util.option_map v.condition
             (fun f -> ("Condition", (Condition.to_json f)))])
    let of_json j =
      {
        condition =
          (Aws.Util.option_map (Aws.Json.lookup j "Condition")
             Condition.of_json);
        redirect =
          (Redirect.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Redirect")))
      }
  end
module RoutingRules =
  struct
    type t = RoutingRule.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map RoutingRule.parse (Aws.Xml.members "RoutingRule" xml))
    let to_query v = Aws.Query.to_query_list RoutingRule.to_query v
    let to_json v = `List (List.map RoutingRule.to_json v)
    let of_json j = Aws.Json.to_list RoutingRule.of_json j
  end
module RedirectAllRequestsTo =
  struct
    type t = {
      host_name: String.t ;
      protocol: Protocol.t option }
    let make ~host_name  ?protocol  () = { host_name; protocol }
    let parse xml =
      Some
        {
          host_name =
            (Aws.Xml.required "HostName"
               (Aws.Util.option_bind (Aws.Xml.member "HostName" xml)
                  String.parse));
          protocol =
            (Aws.Util.option_bind (Aws.Xml.member "Protocol" xml)
               Protocol.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.protocol
              (fun f -> Aws.Query.Pair ("Protocol", (Protocol.to_query f)));
           Some (Aws.Query.Pair ("HostName", (String.to_query v.host_name)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.protocol
              (fun f -> ("Protocol", (Protocol.to_json f)));
           Some ("HostName", (String.to_json v.host_name))])
    let of_json j =
      {
        host_name =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "HostName")));
        protocol =
          (Aws.Util.option_map (Aws.Json.lookup j "Protocol")
             Protocol.of_json)
      }
  end
module IndexDocument =
  struct
    type t = {
      suffix: String.t }
    let make ~suffix  () = { suffix }
    let parse xml =
      Some
        {
          suffix =
            (Aws.Xml.required "Suffix"
               (Aws.Util.option_bind (Aws.Xml.member "Suffix" xml)
                  String.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Suffix", (String.to_query v.suffix)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Suffix", (String.to_json v.suffix))])
    let of_json j =
      {
        suffix =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Suffix")))
      }
  end
module ErrorDocument =
  struct
    type t = {
      key: String.t }
    let make ~key  () = { key }
    let parse xml =
      Some
        {
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Key", (String.to_query v.key)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt [Some ("Key", (String.to_json v.key))])
    let of_json j =
      {
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")))
      }
  end
module WebsiteConfiguration =
  struct
    type t =
      {
      error_document: ErrorDocument.t option ;
      index_document: IndexDocument.t option ;
      redirect_all_requests_to: RedirectAllRequestsTo.t option ;
      routing_rules: RoutingRules.t }
    let make ?error_document  ?index_document  ?redirect_all_requests_to 
      ?(routing_rules= [])  () =
      {
        error_document;
        index_document;
        redirect_all_requests_to;
        routing_rules
      }
    let parse xml =
      Some
        {
          error_document =
            (Aws.Util.option_bind (Aws.Xml.member "ErrorDocument" xml)
               ErrorDocument.parse);
          index_document =
            (Aws.Util.option_bind (Aws.Xml.member "IndexDocument" xml)
               IndexDocument.parse);
          redirect_all_requests_to =
            (Aws.Util.option_bind
               (Aws.Xml.member "RedirectAllRequestsTo" xml)
               RedirectAllRequestsTo.parse);
          routing_rules =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "RoutingRules" xml)
                  RoutingRules.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("RoutingRules.member",
                   (RoutingRules.to_query v.routing_rules)));
           Aws.Util.option_map v.redirect_all_requests_to
             (fun f ->
                Aws.Query.Pair
                  ("RedirectAllRequestsTo",
                    (RedirectAllRequestsTo.to_query f)));
           Aws.Util.option_map v.index_document
             (fun f ->
                Aws.Query.Pair ("IndexDocument", (IndexDocument.to_query f)));
           Aws.Util.option_map v.error_document
             (fun f ->
                Aws.Query.Pair ("ErrorDocument", (ErrorDocument.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("RoutingRules", (RoutingRules.to_json v.routing_rules));
           Aws.Util.option_map v.redirect_all_requests_to
             (fun f ->
                ("RedirectAllRequestsTo", (RedirectAllRequestsTo.to_json f)));
           Aws.Util.option_map v.index_document
             (fun f -> ("IndexDocument", (IndexDocument.to_json f)));
           Aws.Util.option_map v.error_document
             (fun f -> ("ErrorDocument", (ErrorDocument.to_json f)))])
    let of_json j =
      {
        error_document =
          (Aws.Util.option_map (Aws.Json.lookup j "ErrorDocument")
             ErrorDocument.of_json);
        index_document =
          (Aws.Util.option_map (Aws.Json.lookup j "IndexDocument")
             IndexDocument.of_json);
        redirect_all_requests_to =
          (Aws.Util.option_map (Aws.Json.lookup j "RedirectAllRequestsTo")
             RedirectAllRequestsTo.of_json);
        routing_rules =
          (RoutingRules.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "RoutingRules")))
      }
  end
module ObjectPart =
  struct
    type t =
      {
      part_number: Integer.t option ;
      size: Integer.t option ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option }
    let make ?part_number  ?size  ?checksum_c_r_c32  ?checksum_c_r_c32_c 
      ?checksum_s_h_a1  ?checksum_s_h_a256  () =
      {
        part_number;
        size;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256
      }
    let parse xml =
      Some
        {
          part_number =
            (Aws.Util.option_bind (Aws.Xml.member "PartNumber" xml)
               Integer.parse);
          size =
            (Aws.Util.option_bind (Aws.Xml.member "Size" xml) Integer.parse);
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32C" xml)
               String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA256" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_s_h_a256
              (fun f ->
                 Aws.Query.Pair ("ChecksumSHA256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> Aws.Query.Pair ("ChecksumSHA1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> Aws.Query.Pair ("ChecksumCRC32C", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> Aws.Query.Pair ("ChecksumCRC32", (String.to_query f)));
           Aws.Util.option_map v.size
             (fun f -> Aws.Query.Pair ("Size", (Integer.to_query f)));
           Aws.Util.option_map v.part_number
             (fun f -> Aws.Query.Pair ("PartNumber", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_s_h_a256
              (fun f -> ("ChecksumSHA256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("ChecksumSHA1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("ChecksumCRC32C", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("ChecksumCRC32", (String.to_json f)));
           Aws.Util.option_map v.size
             (fun f -> ("Size", (Integer.to_json f)));
           Aws.Util.option_map v.part_number
             (fun f -> ("PartNumber", (Integer.to_json f)))])
    let of_json j =
      {
        part_number =
          (Aws.Util.option_map (Aws.Json.lookup j "PartNumber")
             Integer.of_json);
        size =
          (Aws.Util.option_map (Aws.Json.lookup j "Size") Integer.of_json);
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32C")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA256")
             String.of_json)
      }
  end
module PartsList =
  struct
    type t = ObjectPart.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map ObjectPart.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list ObjectPart.to_query v
    let to_json v = `List (List.map ObjectPart.to_json v)
    let of_json j = Aws.Json.to_list ObjectPart.of_json j
  end
module GetObjectAttributesParts =
  struct
    type t =
      {
      total_parts_count: Integer.t option ;
      part_number_marker: Integer.t option ;
      next_part_number_marker: Integer.t option ;
      max_parts: Integer.t option ;
      is_truncated: Boolean.t option ;
      parts: PartsList.t }
    let make ?total_parts_count  ?part_number_marker 
      ?next_part_number_marker  ?max_parts  ?is_truncated  ?(parts= [])  () =
      {
        total_parts_count;
        part_number_marker;
        next_part_number_marker;
        max_parts;
        is_truncated;
        parts
      }
    let parse xml =
      Some
        {
          total_parts_count =
            (Aws.Util.option_bind (Aws.Xml.member "PartsCount" xml)
               Integer.parse);
          part_number_marker =
            (Aws.Util.option_bind (Aws.Xml.member "PartNumberMarker" xml)
               Integer.parse);
          next_part_number_marker =
            (Aws.Util.option_bind (Aws.Xml.member "NextPartNumberMarker" xml)
               Integer.parse);
          max_parts =
            (Aws.Util.option_bind (Aws.Xml.member "MaxParts" xml)
               Integer.parse);
          is_truncated =
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml)
               Boolean.parse);
          parts = (Aws.Util.of_option [] (PartsList.parse xml))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Part", (PartsList.to_query v.parts)));
           Aws.Util.option_map v.is_truncated
             (fun f -> Aws.Query.Pair ("IsTruncated", (Boolean.to_query f)));
           Aws.Util.option_map v.max_parts
             (fun f -> Aws.Query.Pair ("MaxParts", (Integer.to_query f)));
           Aws.Util.option_map v.next_part_number_marker
             (fun f ->
                Aws.Query.Pair ("NextPartNumberMarker", (Integer.to_query f)));
           Aws.Util.option_map v.part_number_marker
             (fun f ->
                Aws.Query.Pair ("PartNumberMarker", (Integer.to_query f)));
           Aws.Util.option_map v.total_parts_count
             (fun f -> Aws.Query.Pair ("PartsCount", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Part", (PartsList.to_json v.parts));
           Aws.Util.option_map v.is_truncated
             (fun f -> ("IsTruncated", (Boolean.to_json f)));
           Aws.Util.option_map v.max_parts
             (fun f -> ("MaxParts", (Integer.to_json f)));
           Aws.Util.option_map v.next_part_number_marker
             (fun f -> ("NextPartNumberMarker", (Integer.to_json f)));
           Aws.Util.option_map v.part_number_marker
             (fun f -> ("PartNumberMarker", (Integer.to_json f)));
           Aws.Util.option_map v.total_parts_count
             (fun f -> ("PartsCount", (Integer.to_json f)))])
    let of_json j =
      {
        total_parts_count =
          (Aws.Util.option_map (Aws.Json.lookup j "PartsCount")
             Integer.of_json);
        part_number_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "PartNumberMarker")
             Integer.of_json);
        next_part_number_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "NextPartNumberMarker")
             Integer.of_json);
        max_parts =
          (Aws.Util.option_map (Aws.Json.lookup j "MaxParts") Integer.of_json);
        is_truncated =
          (Aws.Util.option_map (Aws.Json.lookup j "IsTruncated")
             Boolean.of_json);
        parts =
          (PartsList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Part")))
      }
  end
module Checksum =
  struct
    type t =
      {
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option }
    let make ?checksum_c_r_c32  ?checksum_c_r_c32_c  ?checksum_s_h_a1 
      ?checksum_s_h_a256  () =
      {
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256
      }
    let parse xml =
      Some
        {
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32C" xml)
               String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA256" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_s_h_a256
              (fun f ->
                 Aws.Query.Pair ("ChecksumSHA256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> Aws.Query.Pair ("ChecksumSHA1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> Aws.Query.Pair ("ChecksumCRC32C", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> Aws.Query.Pair ("ChecksumCRC32", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_s_h_a256
              (fun f -> ("ChecksumSHA256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("ChecksumSHA1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("ChecksumCRC32C", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("ChecksumCRC32", (String.to_json f)))])
    let of_json j =
      {
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32C")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA256")
             String.of_json)
      }
  end
module GetObjectAttributesOutput =
  struct
    type t =
      {
      delete_marker: Boolean.t option ;
      last_modified: DateTime.t option ;
      version_id: String.t option ;
      request_charged: RequestCharged.t option ;
      e_tag: String.t option ;
      checksum: Checksum.t option ;
      object_parts: GetObjectAttributesParts.t option ;
      storage_class: StorageClass.t option ;
      object_size: Long.t option }
    let make ?delete_marker  ?last_modified  ?version_id  ?request_charged 
      ?e_tag  ?checksum  ?object_parts  ?storage_class  ?object_size  () =
      {
        delete_marker;
        last_modified;
        version_id;
        request_charged;
        e_tag;
        checksum;
        object_parts;
        storage_class;
        object_size
      }
    let parse xml =
      Some
        {
          delete_marker =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-delete-marker" xml)
               Boolean.parse);
          last_modified =
            (Aws.Util.option_bind (Aws.Xml.member "Last-Modified" xml)
               DateTime.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-version-id" xml)
               String.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse);
          e_tag =
            (Aws.Util.option_bind (Aws.Xml.member "ETag" xml) String.parse);
          checksum =
            (Aws.Util.option_bind (Aws.Xml.member "Checksum" xml)
               Checksum.parse);
          object_parts =
            (Aws.Util.option_bind (Aws.Xml.member "ObjectParts" xml)
               GetObjectAttributesParts.parse);
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "StorageClass" xml)
               StorageClass.parse);
          object_size =
            (Aws.Util.option_bind (Aws.Xml.member "ObjectSize" xml)
               Long.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.object_size
              (fun f -> Aws.Query.Pair ("ObjectSize", (Long.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair ("StorageClass", (StorageClass.to_query f)));
           Aws.Util.option_map v.object_parts
             (fun f ->
                Aws.Query.Pair
                  ("ObjectParts", (GetObjectAttributesParts.to_query f)));
           Aws.Util.option_map v.checksum
             (fun f -> Aws.Query.Pair ("Checksum", (Checksum.to_query f)));
           Aws.Util.option_map v.e_tag
             (fun f -> Aws.Query.Pair ("ETag", (String.to_query f)));
           Aws.Util.option_map v.request_charged
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f ->
                Aws.Query.Pair ("x-amz-version-id", (String.to_query f)));
           Aws.Util.option_map v.last_modified
             (fun f ->
                Aws.Query.Pair ("Last-Modified", (DateTime.to_query f)));
           Aws.Util.option_map v.delete_marker
             (fun f ->
                Aws.Query.Pair ("x-amz-delete-marker", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.object_size
              (fun f -> ("ObjectSize", (Long.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f -> ("StorageClass", (StorageClass.to_json f)));
           Aws.Util.option_map v.object_parts
             (fun f -> ("ObjectParts", (GetObjectAttributesParts.to_json f)));
           Aws.Util.option_map v.checksum
             (fun f -> ("Checksum", (Checksum.to_json f)));
           Aws.Util.option_map v.e_tag
             (fun f -> ("ETag", (String.to_json f)));
           Aws.Util.option_map v.request_charged
             (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("x-amz-version-id", (String.to_json f)));
           Aws.Util.option_map v.last_modified
             (fun f -> ("Last-Modified", (DateTime.to_json f)));
           Aws.Util.option_map v.delete_marker
             (fun f -> ("x-amz-delete-marker", (Boolean.to_json f)))])
    let of_json j =
      {
        delete_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-delete-marker")
             Boolean.of_json);
        last_modified =
          (Aws.Util.option_map (Aws.Json.lookup j "Last-Modified")
             DateTime.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-version-id")
             String.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json);
        e_tag =
          (Aws.Util.option_map (Aws.Json.lookup j "ETag") String.of_json);
        checksum =
          (Aws.Util.option_map (Aws.Json.lookup j "Checksum")
             Checksum.of_json);
        object_parts =
          (Aws.Util.option_map (Aws.Json.lookup j "ObjectParts")
             GetObjectAttributesParts.of_json);
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "StorageClass")
             StorageClass.of_json);
        object_size =
          (Aws.Util.option_map (Aws.Json.lookup j "ObjectSize") Long.of_json)
      }
  end
module JSONType =
  struct
    type t =
      | DOCUMENT 
      | LINES 
    let str_to_t = [("LINES", LINES); ("DOCUMENT", DOCUMENT)]
    let t_to_str = [(LINES, "LINES"); (DOCUMENT, "DOCUMENT")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module TaggingDirective =
  struct
    type t =
      | COPY 
      | REPLACE 
    let str_to_t = [("REPLACE", REPLACE); ("COPY", COPY)]
    let t_to_str = [(REPLACE, "REPLACE"); (COPY, "COPY")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module OptionalObjectAttributes =
  struct
    type t =
      | RestoreStatus 
    let str_to_t = [("RestoreStatus", RestoreStatus)]
    let t_to_str = [(RestoreStatus, "RestoreStatus")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module OptionalObjectAttributesList =
  struct
    type t = OptionalObjectAttributes.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map OptionalObjectAttributes.parse
           (Aws.Xml.members "member" xml))
    let to_query v =
      Aws.Query.to_query_list OptionalObjectAttributes.to_query v
    let to_json v = `List (List.map OptionalObjectAttributes.to_json v)
    let of_json j = Aws.Json.to_list OptionalObjectAttributes.of_json j
  end
module ListObjectVersionsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      delimiter: String.t option ;
      encoding_type: EncodingType.t option ;
      key_marker: String.t option ;
      max_keys: Integer.t option ;
      prefix: String.t option ;
      version_id_marker: String.t option ;
      expected_bucket_owner: String.t option ;
      request_payer: RequestPayer.t option ;
      optional_object_attributes: OptionalObjectAttributesList.t }
    let make ~bucket  ?delimiter  ?encoding_type  ?key_marker  ?max_keys 
      ?prefix  ?version_id_marker  ?expected_bucket_owner  ?request_payer 
      ?(optional_object_attributes= [])  () =
      {
        bucket;
        delimiter;
        encoding_type;
        key_marker;
        max_keys;
        prefix;
        version_id_marker;
        expected_bucket_owner;
        request_payer;
        optional_object_attributes
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          delimiter =
            (Aws.Util.option_bind (Aws.Xml.member "delimiter" xml)
               String.parse);
          encoding_type =
            (Aws.Util.option_bind (Aws.Xml.member "encoding-type" xml)
               EncodingType.parse);
          key_marker =
            (Aws.Util.option_bind (Aws.Xml.member "key-marker" xml)
               String.parse);
          max_keys =
            (Aws.Util.option_bind (Aws.Xml.member "max-keys" xml)
               Integer.parse);
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "prefix" xml) String.parse);
          version_id_marker =
            (Aws.Util.option_bind (Aws.Xml.member "version-id-marker" xml)
               String.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          optional_object_attributes =
            (Aws.Util.of_option []
               (Aws.Util.option_bind
                  (Aws.Xml.member "x-amz-optional-object-attributes" xml)
                  OptionalObjectAttributesList.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("x-amz-optional-object-attributes",
                   (OptionalObjectAttributesList.to_query
                      v.optional_object_attributes)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.version_id_marker
             (fun f ->
                Aws.Query.Pair ("version-id-marker", (String.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("prefix", (String.to_query f)));
           Aws.Util.option_map v.max_keys
             (fun f -> Aws.Query.Pair ("max-keys", (Integer.to_query f)));
           Aws.Util.option_map v.key_marker
             (fun f -> Aws.Query.Pair ("key-marker", (String.to_query f)));
           Aws.Util.option_map v.encoding_type
             (fun f ->
                Aws.Query.Pair ("encoding-type", (EncodingType.to_query f)));
           Aws.Util.option_map v.delimiter
             (fun f -> Aws.Query.Pair ("delimiter", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("x-amz-optional-object-attributes",
                (OptionalObjectAttributesList.to_json
                   v.optional_object_attributes));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.version_id_marker
             (fun f -> ("version-id-marker", (String.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("prefix", (String.to_json f)));
           Aws.Util.option_map v.max_keys
             (fun f -> ("max-keys", (Integer.to_json f)));
           Aws.Util.option_map v.key_marker
             (fun f -> ("key-marker", (String.to_json f)));
           Aws.Util.option_map v.encoding_type
             (fun f -> ("encoding-type", (EncodingType.to_json f)));
           Aws.Util.option_map v.delimiter
             (fun f -> ("delimiter", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        delimiter =
          (Aws.Util.option_map (Aws.Json.lookup j "delimiter") String.of_json);
        encoding_type =
          (Aws.Util.option_map (Aws.Json.lookup j "encoding-type")
             EncodingType.of_json);
        key_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "key-marker")
             String.of_json);
        max_keys =
          (Aws.Util.option_map (Aws.Json.lookup j "max-keys") Integer.of_json);
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "prefix") String.of_json);
        version_id_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "version-id-marker")
             String.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        optional_object_attributes =
          (OptionalObjectAttributesList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "x-amz-optional-object-attributes")))
      }
  end
module Owner =
  struct
    type t = {
      display_name: String.t option ;
      i_d: String.t option }
    let make ?display_name  ?i_d  () = { display_name; i_d }
    let parse xml =
      Some
        {
          display_name =
            (Aws.Util.option_bind (Aws.Xml.member "DisplayName" xml)
               String.parse);
          i_d = (Aws.Util.option_bind (Aws.Xml.member "ID" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.i_d
              (fun f -> Aws.Query.Pair ("ID", (String.to_query f)));
           Aws.Util.option_map v.display_name
             (fun f -> Aws.Query.Pair ("DisplayName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.i_d (fun f -> ("ID", (String.to_json f)));
           Aws.Util.option_map v.display_name
             (fun f -> ("DisplayName", (String.to_json f)))])
    let of_json j =
      {
        display_name =
          (Aws.Util.option_map (Aws.Json.lookup j "DisplayName")
             String.of_json);
        i_d = (Aws.Util.option_map (Aws.Json.lookup j "ID") String.of_json)
      }
  end
module MultipartUpload =
  struct
    type t =
      {
      upload_id: String.t option ;
      key: String.t option ;
      initiated: DateTime.t option ;
      storage_class: StorageClass.t option ;
      owner: Owner.t option ;
      initiator: Initiator.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option }
    let make ?upload_id  ?key  ?initiated  ?storage_class  ?owner  ?initiator
       ?checksum_algorithm  () =
      {
        upload_id;
        key;
        initiated;
        storage_class;
        owner;
        initiator;
        checksum_algorithm
      }
    let parse xml =
      Some
        {
          upload_id =
            (Aws.Util.option_bind (Aws.Xml.member "UploadId" xml)
               String.parse);
          key =
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse);
          initiated =
            (Aws.Util.option_bind (Aws.Xml.member "Initiated" xml)
               DateTime.parse);
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "StorageClass" xml)
               StorageClass.parse);
          owner =
            (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) Owner.parse);
          initiator =
            (Aws.Util.option_bind (Aws.Xml.member "Initiator" xml)
               Initiator.parse);
          checksum_algorithm =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumAlgorithm" xml)
               ChecksumAlgorithm.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_algorithm
              (fun f ->
                 Aws.Query.Pair
                   ("ChecksumAlgorithm", (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.initiator
             (fun f -> Aws.Query.Pair ("Initiator", (Initiator.to_query f)));
           Aws.Util.option_map v.owner
             (fun f -> Aws.Query.Pair ("Owner", (Owner.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair ("StorageClass", (StorageClass.to_query f)));
           Aws.Util.option_map v.initiated
             (fun f -> Aws.Query.Pair ("Initiated", (DateTime.to_query f)));
           Aws.Util.option_map v.key
             (fun f -> Aws.Query.Pair ("Key", (String.to_query f)));
           Aws.Util.option_map v.upload_id
             (fun f -> Aws.Query.Pair ("UploadId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_algorithm
              (fun f -> ("ChecksumAlgorithm", (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.initiator
             (fun f -> ("Initiator", (Initiator.to_json f)));
           Aws.Util.option_map v.owner
             (fun f -> ("Owner", (Owner.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f -> ("StorageClass", (StorageClass.to_json f)));
           Aws.Util.option_map v.initiated
             (fun f -> ("Initiated", (DateTime.to_json f)));
           Aws.Util.option_map v.key (fun f -> ("Key", (String.to_json f)));
           Aws.Util.option_map v.upload_id
             (fun f -> ("UploadId", (String.to_json f)))])
    let of_json j =
      {
        upload_id =
          (Aws.Util.option_map (Aws.Json.lookup j "UploadId") String.of_json);
        key = (Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json);
        initiated =
          (Aws.Util.option_map (Aws.Json.lookup j "Initiated")
             DateTime.of_json);
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "StorageClass")
             StorageClass.of_json);
        owner =
          (Aws.Util.option_map (Aws.Json.lookup j "Owner") Owner.of_json);
        initiator =
          (Aws.Util.option_map (Aws.Json.lookup j "Initiator")
             Initiator.of_json);
        checksum_algorithm =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumAlgorithm")
             ChecksumAlgorithm.of_json)
      }
  end
module MultipartUploadList =
  struct
    type t = MultipartUpload.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map MultipartUpload.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list MultipartUpload.to_query v
    let to_json v = `List (List.map MultipartUpload.to_json v)
    let of_json j = Aws.Json.to_list MultipartUpload.of_json j
  end
module CommonPrefix =
  struct
    type t = {
      prefix: String.t option }
    let make ?prefix  () = { prefix }
    let parse xml =
      Some
        {
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.prefix
              (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.prefix
              (fun f -> ("Prefix", (String.to_json f)))])
    let of_json j =
      {
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json)
      }
  end
module CommonPrefixList =
  struct
    type t = CommonPrefix.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map CommonPrefix.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list CommonPrefix.to_query v
    let to_json v = `List (List.map CommonPrefix.to_json v)
    let of_json j = Aws.Json.to_list CommonPrefix.of_json j
  end
module ListMultipartUploadsOutput =
  struct
    type t =
      {
      bucket: String.t option ;
      key_marker: String.t option ;
      upload_id_marker: String.t option ;
      next_key_marker: String.t option ;
      prefix: String.t option ;
      delimiter: String.t option ;
      next_upload_id_marker: String.t option ;
      max_uploads: Integer.t option ;
      is_truncated: Boolean.t option ;
      uploads: MultipartUploadList.t ;
      common_prefixes: CommonPrefixList.t ;
      encoding_type: EncodingType.t option ;
      request_charged: RequestCharged.t option }
    let make ?bucket  ?key_marker  ?upload_id_marker  ?next_key_marker 
      ?prefix  ?delimiter  ?next_upload_id_marker  ?max_uploads 
      ?is_truncated  ?(uploads= [])  ?(common_prefixes= [])  ?encoding_type 
      ?request_charged  () =
      {
        bucket;
        key_marker;
        upload_id_marker;
        next_key_marker;
        prefix;
        delimiter;
        next_upload_id_marker;
        max_uploads;
        is_truncated;
        uploads;
        common_prefixes;
        encoding_type;
        request_charged
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml) String.parse);
          key_marker =
            (Aws.Util.option_bind (Aws.Xml.member "KeyMarker" xml)
               String.parse);
          upload_id_marker =
            (Aws.Util.option_bind (Aws.Xml.member "UploadIdMarker" xml)
               String.parse);
          next_key_marker =
            (Aws.Util.option_bind (Aws.Xml.member "NextKeyMarker" xml)
               String.parse);
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          delimiter =
            (Aws.Util.option_bind (Aws.Xml.member "Delimiter" xml)
               String.parse);
          next_upload_id_marker =
            (Aws.Util.option_bind (Aws.Xml.member "NextUploadIdMarker" xml)
               String.parse);
          max_uploads =
            (Aws.Util.option_bind (Aws.Xml.member "MaxUploads" xml)
               Integer.parse);
          is_truncated =
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml)
               Boolean.parse);
          uploads = (Aws.Util.of_option [] (MultipartUploadList.parse xml));
          common_prefixes =
            (Aws.Util.of_option [] (CommonPrefixList.parse xml));
          encoding_type =
            (Aws.Util.option_bind (Aws.Xml.member "EncodingType" xml)
               EncodingType.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.encoding_type
             (fun f ->
                Aws.Query.Pair ("EncodingType", (EncodingType.to_query f)));
           Some
             (Aws.Query.Pair
                ("CommonPrefixes.member",
                  (CommonPrefixList.to_query v.common_prefixes)));
           Some
             (Aws.Query.Pair
                ("Upload", (MultipartUploadList.to_query v.uploads)));
           Aws.Util.option_map v.is_truncated
             (fun f -> Aws.Query.Pair ("IsTruncated", (Boolean.to_query f)));
           Aws.Util.option_map v.max_uploads
             (fun f -> Aws.Query.Pair ("MaxUploads", (Integer.to_query f)));
           Aws.Util.option_map v.next_upload_id_marker
             (fun f ->
                Aws.Query.Pair ("NextUploadIdMarker", (String.to_query f)));
           Aws.Util.option_map v.delimiter
             (fun f -> Aws.Query.Pair ("Delimiter", (String.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)));
           Aws.Util.option_map v.next_key_marker
             (fun f -> Aws.Query.Pair ("NextKeyMarker", (String.to_query f)));
           Aws.Util.option_map v.upload_id_marker
             (fun f -> Aws.Query.Pair ("UploadIdMarker", (String.to_query f)));
           Aws.Util.option_map v.key_marker
             (fun f -> Aws.Query.Pair ("KeyMarker", (String.to_query f)));
           Aws.Util.option_map v.bucket
             (fun f -> Aws.Query.Pair ("Bucket", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.encoding_type
             (fun f -> ("EncodingType", (EncodingType.to_json f)));
           Some
             ("CommonPrefixes", (CommonPrefixList.to_json v.common_prefixes));
           Some ("Upload", (MultipartUploadList.to_json v.uploads));
           Aws.Util.option_map v.is_truncated
             (fun f -> ("IsTruncated", (Boolean.to_json f)));
           Aws.Util.option_map v.max_uploads
             (fun f -> ("MaxUploads", (Integer.to_json f)));
           Aws.Util.option_map v.next_upload_id_marker
             (fun f -> ("NextUploadIdMarker", (String.to_json f)));
           Aws.Util.option_map v.delimiter
             (fun f -> ("Delimiter", (String.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)));
           Aws.Util.option_map v.next_key_marker
             (fun f -> ("NextKeyMarker", (String.to_json f)));
           Aws.Util.option_map v.upload_id_marker
             (fun f -> ("UploadIdMarker", (String.to_json f)));
           Aws.Util.option_map v.key_marker
             (fun f -> ("KeyMarker", (String.to_json f)));
           Aws.Util.option_map v.bucket
             (fun f -> ("Bucket", (String.to_json f)))])
    let of_json j =
      {
        bucket =
          (Aws.Util.option_map (Aws.Json.lookup j "Bucket") String.of_json);
        key_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "KeyMarker") String.of_json);
        upload_id_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "UploadIdMarker")
             String.of_json);
        next_key_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "NextKeyMarker")
             String.of_json);
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        delimiter =
          (Aws.Util.option_map (Aws.Json.lookup j "Delimiter") String.of_json);
        next_upload_id_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "NextUploadIdMarker")
             String.of_json);
        max_uploads =
          (Aws.Util.option_map (Aws.Json.lookup j "MaxUploads")
             Integer.of_json);
        is_truncated =
          (Aws.Util.option_map (Aws.Json.lookup j "IsTruncated")
             Boolean.of_json);
        uploads =
          (MultipartUploadList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Upload")));
        common_prefixes =
          (CommonPrefixList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "CommonPrefixes")));
        encoding_type =
          (Aws.Util.option_map (Aws.Json.lookup j "EncodingType")
             EncodingType.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module ObjectVersionStorageClass =
  struct
    type t =
      | STANDARD 
    let str_to_t = [("STANDARD", STANDARD)]
    let t_to_str = [(STANDARD, "STANDARD")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module DefaultRetention =
  struct
    type t =
      {
      mode: ObjectLockRetentionMode.t option ;
      days: Integer.t option ;
      years: Integer.t option }
    let make ?mode  ?days  ?years  () = { mode; days; years }
    let parse xml =
      Some
        {
          mode =
            (Aws.Util.option_bind (Aws.Xml.member "Mode" xml)
               ObjectLockRetentionMode.parse);
          days =
            (Aws.Util.option_bind (Aws.Xml.member "Days" xml) Integer.parse);
          years =
            (Aws.Util.option_bind (Aws.Xml.member "Years" xml) Integer.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.years
              (fun f -> Aws.Query.Pair ("Years", (Integer.to_query f)));
           Aws.Util.option_map v.days
             (fun f -> Aws.Query.Pair ("Days", (Integer.to_query f)));
           Aws.Util.option_map v.mode
             (fun f ->
                Aws.Query.Pair ("Mode", (ObjectLockRetentionMode.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.years
              (fun f -> ("Years", (Integer.to_json f)));
           Aws.Util.option_map v.days
             (fun f -> ("Days", (Integer.to_json f)));
           Aws.Util.option_map v.mode
             (fun f -> ("Mode", (ObjectLockRetentionMode.to_json f)))])
    let of_json j =
      {
        mode =
          (Aws.Util.option_map (Aws.Json.lookup j "Mode")
             ObjectLockRetentionMode.of_json);
        days =
          (Aws.Util.option_map (Aws.Json.lookup j "Days") Integer.of_json);
        years =
          (Aws.Util.option_map (Aws.Json.lookup j "Years") Integer.of_json)
      }
  end
module ObjectLockRule =
  struct
    type t = {
      default_retention: DefaultRetention.t option }
    let make ?default_retention  () = { default_retention }
    let parse xml =
      Some
        {
          default_retention =
            (Aws.Util.option_bind (Aws.Xml.member "DefaultRetention" xml)
               DefaultRetention.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.default_retention
              (fun f ->
                 Aws.Query.Pair
                   ("DefaultRetention", (DefaultRetention.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.default_retention
              (fun f -> ("DefaultRetention", (DefaultRetention.to_json f)))])
    let of_json j =
      {
        default_retention =
          (Aws.Util.option_map (Aws.Json.lookup j "DefaultRetention")
             DefaultRetention.of_json)
      }
  end
module IntelligentTieringAccessTier =
  struct
    type t =
      | ARCHIVE_ACCESS 
      | DEEP_ARCHIVE_ACCESS 
    let str_to_t =
      [("DEEP_ARCHIVE_ACCESS", DEEP_ARCHIVE_ACCESS);
      ("ARCHIVE_ACCESS", ARCHIVE_ACCESS)]
    let t_to_str =
      [(DEEP_ARCHIVE_ACCESS, "DEEP_ARCHIVE_ACCESS");
      (ARCHIVE_ACCESS, "ARCHIVE_ACCESS")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module Tiering =
  struct
    type t = {
      days: Integer.t ;
      access_tier: IntelligentTieringAccessTier.t }
    let make ~days  ~access_tier  () = { days; access_tier }
    let parse xml =
      Some
        {
          days =
            (Aws.Xml.required "Days"
               (Aws.Util.option_bind (Aws.Xml.member "Days" xml)
                  Integer.parse));
          access_tier =
            (Aws.Xml.required "AccessTier"
               (Aws.Util.option_bind (Aws.Xml.member "AccessTier" xml)
                  IntelligentTieringAccessTier.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("AccessTier",
                   (IntelligentTieringAccessTier.to_query v.access_tier)));
           Some (Aws.Query.Pair ("Days", (Integer.to_query v.days)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("AccessTier",
                (IntelligentTieringAccessTier.to_json v.access_tier));
           Some ("Days", (Integer.to_json v.days))])
    let of_json j =
      {
        days =
          (Integer.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Days")));
        access_tier =
          (IntelligentTieringAccessTier.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AccessTier")))
      }
  end
module TieringList =
  struct
    type t = Tiering.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map Tiering.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list Tiering.to_query v
    let to_json v = `List (List.map Tiering.to_json v)
    let of_json j = Aws.Json.to_list Tiering.of_json j
  end
module DeletedObject =
  struct
    type t =
      {
      key: String.t option ;
      version_id: String.t option ;
      delete_marker: Boolean.t option ;
      delete_marker_version_id: String.t option }
    let make ?key  ?version_id  ?delete_marker  ?delete_marker_version_id  ()
      = { key; version_id; delete_marker; delete_marker_version_id }
    let parse xml =
      Some
        {
          key =
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "VersionId" xml)
               String.parse);
          delete_marker =
            (Aws.Util.option_bind (Aws.Xml.member "DeleteMarker" xml)
               Boolean.parse);
          delete_marker_version_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "DeleteMarkerVersionId" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.delete_marker_version_id
              (fun f ->
                 Aws.Query.Pair
                   ("DeleteMarkerVersionId", (String.to_query f)));
           Aws.Util.option_map v.delete_marker
             (fun f -> Aws.Query.Pair ("DeleteMarker", (Boolean.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("VersionId", (String.to_query f)));
           Aws.Util.option_map v.key
             (fun f -> Aws.Query.Pair ("Key", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.delete_marker_version_id
              (fun f -> ("DeleteMarkerVersionId", (String.to_json f)));
           Aws.Util.option_map v.delete_marker
             (fun f -> ("DeleteMarker", (Boolean.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("VersionId", (String.to_json f)));
           Aws.Util.option_map v.key (fun f -> ("Key", (String.to_json f)))])
    let of_json j =
      {
        key = (Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "VersionId") String.of_json);
        delete_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "DeleteMarker")
             Boolean.of_json);
        delete_marker_version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "DeleteMarkerVersionId")
             String.of_json)
      }
  end
module CompletedPart =
  struct
    type t =
      {
      e_tag: String.t option ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option ;
      part_number: Integer.t option }
    let make ?e_tag  ?checksum_c_r_c32  ?checksum_c_r_c32_c  ?checksum_s_h_a1
       ?checksum_s_h_a256  ?part_number  () =
      {
        e_tag;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256;
        part_number
      }
    let parse xml =
      Some
        {
          e_tag =
            (Aws.Util.option_bind (Aws.Xml.member "ETag" xml) String.parse);
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32C" xml)
               String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA256" xml)
               String.parse);
          part_number =
            (Aws.Util.option_bind (Aws.Xml.member "PartNumber" xml)
               Integer.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.part_number
              (fun f -> Aws.Query.Pair ("PartNumber", (Integer.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f -> Aws.Query.Pair ("ChecksumSHA256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> Aws.Query.Pair ("ChecksumSHA1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> Aws.Query.Pair ("ChecksumCRC32C", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> Aws.Query.Pair ("ChecksumCRC32", (String.to_query f)));
           Aws.Util.option_map v.e_tag
             (fun f -> Aws.Query.Pair ("ETag", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.part_number
              (fun f -> ("PartNumber", (Integer.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f -> ("ChecksumSHA256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("ChecksumSHA1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("ChecksumCRC32C", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("ChecksumCRC32", (String.to_json f)));
           Aws.Util.option_map v.e_tag
             (fun f -> ("ETag", (String.to_json f)))])
    let of_json j =
      {
        e_tag =
          (Aws.Util.option_map (Aws.Json.lookup j "ETag") String.of_json);
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32C")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA256")
             String.of_json);
        part_number =
          (Aws.Util.option_map (Aws.Json.lookup j "PartNumber")
             Integer.of_json)
      }
  end
module CompletedPartList =
  struct
    type t = CompletedPart.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map CompletedPart.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list CompletedPart.to_query v
    let to_json v = `List (List.map CompletedPart.to_json v)
    let of_json j = Aws.Json.to_list CompletedPart.of_json j
  end
module CompletedMultipartUpload =
  struct
    type t = {
      parts: CompletedPartList.t }
    let make ?(parts= [])  () = { parts }
    let parse xml =
      Some { parts = (Aws.Util.of_option [] (CompletedPartList.parse xml)) }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair ("Part", (CompletedPartList.to_query v.parts)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Part", (CompletedPartList.to_json v.parts))])
    let of_json j =
      {
        parts =
          (CompletedPartList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Part")))
      }
  end
module IntelligentTieringStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)]
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module IntelligentTieringAndOperator =
  struct
    type t = {
      prefix: String.t option ;
      tags: TagSet.t }
    let make ?prefix  ?(tags= [])  () = { prefix; tags }
    let parse xml =
      Some
        {
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          tags =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "Tag" xml) TagSet.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Tag", (TagSet.to_query v.tags)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Tag", (TagSet.to_json v.tags));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)))])
    let of_json j =
      {
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        tags =
          (TagSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tag")))
      }
  end
module IntelligentTieringFilter =
  struct
    type t =
      {
      prefix: String.t option ;
      tag: Tag.t option ;
      and_: IntelligentTieringAndOperator.t option }
    let make ?prefix  ?tag  ?and_  () = { prefix; tag; and_ }
    let parse xml =
      Some
        {
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          tag = (Aws.Util.option_bind (Aws.Xml.member "Tag" xml) Tag.parse);
          and_ =
            (Aws.Util.option_bind (Aws.Xml.member "And" xml)
               IntelligentTieringAndOperator.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.and_
              (fun f ->
                 Aws.Query.Pair
                   ("And", (IntelligentTieringAndOperator.to_query f)));
           Aws.Util.option_map v.tag
             (fun f -> Aws.Query.Pair ("Tag", (Tag.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.and_
              (fun f -> ("And", (IntelligentTieringAndOperator.to_json f)));
           Aws.Util.option_map v.tag (fun f -> ("Tag", (Tag.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)))])
    let of_json j =
      {
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        tag = (Aws.Util.option_map (Aws.Json.lookup j "Tag") Tag.of_json);
        and_ =
          (Aws.Util.option_map (Aws.Json.lookup j "And")
             IntelligentTieringAndOperator.of_json)
      }
  end
module IntelligentTieringConfiguration =
  struct
    type t =
      {
      id: String.t ;
      filter: IntelligentTieringFilter.t option ;
      status: IntelligentTieringStatus.t ;
      tierings: TieringList.t }
    let make ~id  ?filter  ~status  ~tierings  () =
      { id; filter; status; tierings }
    let parse xml =
      Some
        {
          id =
            (Aws.Xml.required "Id"
               (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse));
          filter =
            (Aws.Util.option_bind (Aws.Xml.member "Filter" xml)
               IntelligentTieringFilter.parse);
          status =
            (Aws.Xml.required "Status"
               (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
                  IntelligentTieringStatus.parse));
          tierings = (Aws.Xml.required "Tiering" (TieringList.parse xml))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair ("Tiering", (TieringList.to_query v.tierings)));
           Some
             (Aws.Query.Pair
                ("Status", (IntelligentTieringStatus.to_query v.status)));
           Aws.Util.option_map v.filter
             (fun f ->
                Aws.Query.Pair
                  ("Filter", (IntelligentTieringFilter.to_query f)));
           Some (Aws.Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Tiering", (TieringList.to_json v.tierings));
           Some ("Status", (IntelligentTieringStatus.to_json v.status));
           Aws.Util.option_map v.filter
             (fun f -> ("Filter", (IntelligentTieringFilter.to_json f)));
           Some ("Id", (String.to_json v.id))])
    let of_json j =
      {
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")));
        filter =
          (Aws.Util.option_map (Aws.Json.lookup j "Filter")
             IntelligentTieringFilter.of_json);
        status =
          (IntelligentTieringStatus.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Status")));
        tierings =
          (TieringList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Tiering")))
      }
  end
module Part =
  struct
    type t =
      {
      part_number: Integer.t option ;
      last_modified: DateTime.t option ;
      e_tag: String.t option ;
      size: Integer.t option ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option }
    let make ?part_number  ?last_modified  ?e_tag  ?size  ?checksum_c_r_c32 
      ?checksum_c_r_c32_c  ?checksum_s_h_a1  ?checksum_s_h_a256  () =
      {
        part_number;
        last_modified;
        e_tag;
        size;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256
      }
    let parse xml =
      Some
        {
          part_number =
            (Aws.Util.option_bind (Aws.Xml.member "PartNumber" xml)
               Integer.parse);
          last_modified =
            (Aws.Util.option_bind (Aws.Xml.member "LastModified" xml)
               DateTime.parse);
          e_tag =
            (Aws.Util.option_bind (Aws.Xml.member "ETag" xml) String.parse);
          size =
            (Aws.Util.option_bind (Aws.Xml.member "Size" xml) Integer.parse);
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32C" xml)
               String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA256" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_s_h_a256
              (fun f ->
                 Aws.Query.Pair ("ChecksumSHA256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> Aws.Query.Pair ("ChecksumSHA1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> Aws.Query.Pair ("ChecksumCRC32C", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> Aws.Query.Pair ("ChecksumCRC32", (String.to_query f)));
           Aws.Util.option_map v.size
             (fun f -> Aws.Query.Pair ("Size", (Integer.to_query f)));
           Aws.Util.option_map v.e_tag
             (fun f -> Aws.Query.Pair ("ETag", (String.to_query f)));
           Aws.Util.option_map v.last_modified
             (fun f -> Aws.Query.Pair ("LastModified", (DateTime.to_query f)));
           Aws.Util.option_map v.part_number
             (fun f -> Aws.Query.Pair ("PartNumber", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_s_h_a256
              (fun f -> ("ChecksumSHA256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("ChecksumSHA1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("ChecksumCRC32C", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("ChecksumCRC32", (String.to_json f)));
           Aws.Util.option_map v.size
             (fun f -> ("Size", (Integer.to_json f)));
           Aws.Util.option_map v.e_tag
             (fun f -> ("ETag", (String.to_json f)));
           Aws.Util.option_map v.last_modified
             (fun f -> ("LastModified", (DateTime.to_json f)));
           Aws.Util.option_map v.part_number
             (fun f -> ("PartNumber", (Integer.to_json f)))])
    let of_json j =
      {
        part_number =
          (Aws.Util.option_map (Aws.Json.lookup j "PartNumber")
             Integer.of_json);
        last_modified =
          (Aws.Util.option_map (Aws.Json.lookup j "LastModified")
             DateTime.of_json);
        e_tag =
          (Aws.Util.option_map (Aws.Json.lookup j "ETag") String.of_json);
        size =
          (Aws.Util.option_map (Aws.Json.lookup j "Size") Integer.of_json);
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32C")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA256")
             String.of_json)
      }
  end
module Parts =
  struct
    type t = Part.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map Part.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list Part.to_query v
    let to_json v = `List (List.map Part.to_json v)
    let of_json j = Aws.Json.to_list Part.of_json j
  end
module ListPartsOutput =
  struct
    type t =
      {
      abort_date: DateTime.t option ;
      abort_rule_id: String.t option ;
      bucket: String.t option ;
      key: String.t option ;
      upload_id: String.t option ;
      part_number_marker: Integer.t option ;
      next_part_number_marker: Integer.t option ;
      max_parts: Integer.t option ;
      is_truncated: Boolean.t option ;
      parts: Parts.t ;
      initiator: Initiator.t option ;
      owner: Owner.t option ;
      storage_class: StorageClass.t option ;
      request_charged: RequestCharged.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option }
    let make ?abort_date  ?abort_rule_id  ?bucket  ?key  ?upload_id 
      ?part_number_marker  ?next_part_number_marker  ?max_parts 
      ?is_truncated  ?(parts= [])  ?initiator  ?owner  ?storage_class 
      ?request_charged  ?checksum_algorithm  () =
      {
        abort_date;
        abort_rule_id;
        bucket;
        key;
        upload_id;
        part_number_marker;
        next_part_number_marker;
        max_parts;
        is_truncated;
        parts;
        initiator;
        owner;
        storage_class;
        request_charged;
        checksum_algorithm
      }
    let parse xml =
      Some
        {
          abort_date =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-abort-date" xml)
               DateTime.parse);
          abort_rule_id =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-abort-rule-id" xml)
               String.parse);
          bucket =
            (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml) String.parse);
          key =
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse);
          upload_id =
            (Aws.Util.option_bind (Aws.Xml.member "UploadId" xml)
               String.parse);
          part_number_marker =
            (Aws.Util.option_bind (Aws.Xml.member "PartNumberMarker" xml)
               Integer.parse);
          next_part_number_marker =
            (Aws.Util.option_bind (Aws.Xml.member "NextPartNumberMarker" xml)
               Integer.parse);
          max_parts =
            (Aws.Util.option_bind (Aws.Xml.member "MaxParts" xml)
               Integer.parse);
          is_truncated =
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml)
               Boolean.parse);
          parts = (Aws.Util.of_option [] (Parts.parse xml));
          initiator =
            (Aws.Util.option_bind (Aws.Xml.member "Initiator" xml)
               Initiator.parse);
          owner =
            (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) Owner.parse);
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "StorageClass" xml)
               StorageClass.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse);
          checksum_algorithm =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumAlgorithm" xml)
               ChecksumAlgorithm.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_algorithm
              (fun f ->
                 Aws.Query.Pair
                   ("ChecksumAlgorithm", (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.request_charged
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair ("StorageClass", (StorageClass.to_query f)));
           Aws.Util.option_map v.owner
             (fun f -> Aws.Query.Pair ("Owner", (Owner.to_query f)));
           Aws.Util.option_map v.initiator
             (fun f -> Aws.Query.Pair ("Initiator", (Initiator.to_query f)));
           Some (Aws.Query.Pair ("Part", (Parts.to_query v.parts)));
           Aws.Util.option_map v.is_truncated
             (fun f -> Aws.Query.Pair ("IsTruncated", (Boolean.to_query f)));
           Aws.Util.option_map v.max_parts
             (fun f -> Aws.Query.Pair ("MaxParts", (Integer.to_query f)));
           Aws.Util.option_map v.next_part_number_marker
             (fun f ->
                Aws.Query.Pair ("NextPartNumberMarker", (Integer.to_query f)));
           Aws.Util.option_map v.part_number_marker
             (fun f ->
                Aws.Query.Pair ("PartNumberMarker", (Integer.to_query f)));
           Aws.Util.option_map v.upload_id
             (fun f -> Aws.Query.Pair ("UploadId", (String.to_query f)));
           Aws.Util.option_map v.key
             (fun f -> Aws.Query.Pair ("Key", (String.to_query f)));
           Aws.Util.option_map v.bucket
             (fun f -> Aws.Query.Pair ("Bucket", (String.to_query f)));
           Aws.Util.option_map v.abort_rule_id
             (fun f ->
                Aws.Query.Pair ("x-amz-abort-rule-id", (String.to_query f)));
           Aws.Util.option_map v.abort_date
             (fun f ->
                Aws.Query.Pair ("x-amz-abort-date", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_algorithm
              (fun f -> ("ChecksumAlgorithm", (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.request_charged
             (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f -> ("StorageClass", (StorageClass.to_json f)));
           Aws.Util.option_map v.owner
             (fun f -> ("Owner", (Owner.to_json f)));
           Aws.Util.option_map v.initiator
             (fun f -> ("Initiator", (Initiator.to_json f)));
           Some ("Part", (Parts.to_json v.parts));
           Aws.Util.option_map v.is_truncated
             (fun f -> ("IsTruncated", (Boolean.to_json f)));
           Aws.Util.option_map v.max_parts
             (fun f -> ("MaxParts", (Integer.to_json f)));
           Aws.Util.option_map v.next_part_number_marker
             (fun f -> ("NextPartNumberMarker", (Integer.to_json f)));
           Aws.Util.option_map v.part_number_marker
             (fun f -> ("PartNumberMarker", (Integer.to_json f)));
           Aws.Util.option_map v.upload_id
             (fun f -> ("UploadId", (String.to_json f)));
           Aws.Util.option_map v.key (fun f -> ("Key", (String.to_json f)));
           Aws.Util.option_map v.bucket
             (fun f -> ("Bucket", (String.to_json f)));
           Aws.Util.option_map v.abort_rule_id
             (fun f -> ("x-amz-abort-rule-id", (String.to_json f)));
           Aws.Util.option_map v.abort_date
             (fun f -> ("x-amz-abort-date", (DateTime.to_json f)))])
    let of_json j =
      {
        abort_date =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-abort-date")
             DateTime.of_json);
        abort_rule_id =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-abort-rule-id")
             String.of_json);
        bucket =
          (Aws.Util.option_map (Aws.Json.lookup j "Bucket") String.of_json);
        key = (Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json);
        upload_id =
          (Aws.Util.option_map (Aws.Json.lookup j "UploadId") String.of_json);
        part_number_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "PartNumberMarker")
             Integer.of_json);
        next_part_number_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "NextPartNumberMarker")
             Integer.of_json);
        max_parts =
          (Aws.Util.option_map (Aws.Json.lookup j "MaxParts") Integer.of_json);
        is_truncated =
          (Aws.Util.option_map (Aws.Json.lookup j "IsTruncated")
             Boolean.of_json);
        parts =
          (Parts.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Part")));
        initiator =
          (Aws.Util.option_map (Aws.Json.lookup j "Initiator")
             Initiator.of_json);
        owner =
          (Aws.Util.option_map (Aws.Json.lookup j "Owner") Owner.of_json);
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "StorageClass")
             StorageClass.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json);
        checksum_algorithm =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumAlgorithm")
             ChecksumAlgorithm.of_json)
      }
  end
module ExistingObjectReplicationStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)]
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ExistingObjectReplication =
  struct
    type t = {
      status: ExistingObjectReplicationStatus.t }
    let make ~status  () = { status }
    let parse xml =
      Some
        {
          status =
            (Aws.Xml.required "Status"
               (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
                  ExistingObjectReplicationStatus.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("Status",
                   (ExistingObjectReplicationStatus.to_query v.status)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("Status", (ExistingObjectReplicationStatus.to_json v.status))])
    let of_json j =
      {
        status =
          (ExistingObjectReplicationStatus.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Status")))
      }
  end
module ReplicationRuleAndOperator =
  struct
    type t = {
      prefix: String.t option ;
      tags: TagSet.t }
    let make ?prefix  ?(tags= [])  () = { prefix; tags }
    let parse xml =
      Some
        {
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          tags =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "Tag" xml) TagSet.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Tag", (TagSet.to_query v.tags)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Tag", (TagSet.to_json v.tags));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)))])
    let of_json j =
      {
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        tags =
          (TagSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tag")))
      }
  end
module MetricsAndOperator =
  struct
    type t =
      {
      prefix: String.t option ;
      tags: TagSet.t ;
      access_point_arn: String.t option }
    let make ?prefix  ?(tags= [])  ?access_point_arn  () =
      { prefix; tags; access_point_arn }
    let parse xml =
      Some
        {
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          tags =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "Tag" xml) TagSet.parse));
          access_point_arn =
            (Aws.Util.option_bind (Aws.Xml.member "AccessPointArn" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.access_point_arn
              (fun f ->
                 Aws.Query.Pair ("AccessPointArn", (String.to_query f)));
           Some (Aws.Query.Pair ("Tag", (TagSet.to_query v.tags)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.access_point_arn
              (fun f -> ("AccessPointArn", (String.to_json f)));
           Some ("Tag", (TagSet.to_json v.tags));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)))])
    let of_json j =
      {
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        tags =
          (TagSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tag")));
        access_point_arn =
          (Aws.Util.option_map (Aws.Json.lookup j "AccessPointArn")
             String.of_json)
      }
  end
module MetricsFilter =
  struct
    type t =
      {
      prefix: String.t option ;
      tag: Tag.t option ;
      access_point_arn: String.t option ;
      and_: MetricsAndOperator.t option }
    let make ?prefix  ?tag  ?access_point_arn  ?and_  () =
      { prefix; tag; access_point_arn; and_ }
    let parse xml =
      Some
        {
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          tag = (Aws.Util.option_bind (Aws.Xml.member "Tag" xml) Tag.parse);
          access_point_arn =
            (Aws.Util.option_bind (Aws.Xml.member "AccessPointArn" xml)
               String.parse);
          and_ =
            (Aws.Util.option_bind (Aws.Xml.member "And" xml)
               MetricsAndOperator.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.and_
              (fun f ->
                 Aws.Query.Pair ("And", (MetricsAndOperator.to_query f)));
           Aws.Util.option_map v.access_point_arn
             (fun f -> Aws.Query.Pair ("AccessPointArn", (String.to_query f)));
           Aws.Util.option_map v.tag
             (fun f -> Aws.Query.Pair ("Tag", (Tag.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.and_
              (fun f -> ("And", (MetricsAndOperator.to_json f)));
           Aws.Util.option_map v.access_point_arn
             (fun f -> ("AccessPointArn", (String.to_json f)));
           Aws.Util.option_map v.tag (fun f -> ("Tag", (Tag.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)))])
    let of_json j =
      {
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        tag = (Aws.Util.option_map (Aws.Json.lookup j "Tag") Tag.of_json);
        access_point_arn =
          (Aws.Util.option_map (Aws.Json.lookup j "AccessPointArn")
             String.of_json);
        and_ =
          (Aws.Util.option_map (Aws.Json.lookup j "And")
             MetricsAndOperator.of_json)
      }
  end
module MetricsConfiguration =
  struct
    type t = {
      id: String.t ;
      filter: MetricsFilter.t option }
    let make ~id  ?filter  () = { id; filter }
    let parse xml =
      Some
        {
          id =
            (Aws.Xml.required "Id"
               (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse));
          filter =
            (Aws.Util.option_bind (Aws.Xml.member "Filter" xml)
               MetricsFilter.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.filter
              (fun f -> Aws.Query.Pair ("Filter", (MetricsFilter.to_query f)));
           Some (Aws.Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.filter
              (fun f -> ("Filter", (MetricsFilter.to_json f)));
           Some ("Id", (String.to_json v.id))])
    let of_json j =
      {
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")));
        filter =
          (Aws.Util.option_map (Aws.Json.lookup j "Filter")
             MetricsFilter.of_json)
      }
  end
module GetBucketMetricsConfigurationOutput =
  struct
    type t = {
      metrics_configuration: MetricsConfiguration.t option }
    let make ?metrics_configuration  () = { metrics_configuration }
    let parse xml =
      Some
        {
          metrics_configuration =
            (Aws.Util.option_bind (Aws.Xml.member "MetricsConfiguration" xml)
               MetricsConfiguration.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.metrics_configuration
              (fun f ->
                 Aws.Query.Pair
                   ("MetricsConfiguration",
                     (MetricsConfiguration.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.metrics_configuration
              (fun f ->
                 ("MetricsConfiguration", (MetricsConfiguration.to_json f)))])
    let of_json j =
      {
        metrics_configuration =
          (Aws.Util.option_map (Aws.Json.lookup j "MetricsConfiguration")
             MetricsConfiguration.of_json)
      }
  end
module PutBucketLifecycleConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      lifecycle_configuration: BucketLifecycleConfiguration.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?checksum_algorithm  ?lifecycle_configuration 
      ?expected_bucket_owner  () =
      {
        bucket;
        checksum_algorithm;
        lifecycle_configuration;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          lifecycle_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "LifecycleConfiguration" xml)
               BucketLifecycleConfiguration.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.lifecycle_configuration
             (fun f ->
                Aws.Query.Pair
                  ("LifecycleConfiguration",
                    (BucketLifecycleConfiguration.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.lifecycle_configuration
             (fun f ->
                ("LifecycleConfiguration",
                  (BucketLifecycleConfiguration.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        lifecycle_configuration =
          (Aws.Util.option_map (Aws.Json.lookup j "LifecycleConfiguration")
             BucketLifecycleConfiguration.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetObjectAclRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      version_id: String.t option ;
      request_payer: RequestPayer.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~key  ?version_id  ?request_payer 
      ?expected_bucket_owner  () =
      { bucket; key; version_id; request_payer; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module BucketCannedACL =
  struct
    type t =
      | Private 
      | Public_read 
      | Public_read_write 
      | Authenticated_read 
    let str_to_t =
      [("authenticated-read", Authenticated_read);
      ("public-read-write", Public_read_write);
      ("public-read", Public_read);
      ("private", Private)]
    let t_to_str =
      [(Authenticated_read, "authenticated-read");
      (Public_read_write, "public-read-write");
      (Public_read, "public-read");
      (Private, "private")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module AccessControlPolicy =
  struct
    type t = {
      grants: Grants.t ;
      owner: Owner.t option }
    let make ?(grants= [])  ?owner  () = { grants; owner }
    let parse xml =
      Some
        {
          grants =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "AccessControlList" xml)
                  Grants.parse));
          owner =
            (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) Owner.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.owner
              (fun f -> Aws.Query.Pair ("Owner", (Owner.to_query f)));
           Some
             (Aws.Query.Pair
                ("AccessControlList", (Grants.to_query v.grants)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.owner
              (fun f -> ("Owner", (Owner.to_json f)));
           Some ("AccessControlList", (Grants.to_json v.grants))])
    let of_json j =
      {
        grants =
          (Grants.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AccessControlList")));
        owner =
          (Aws.Util.option_map (Aws.Json.lookup j "Owner") Owner.of_json)
      }
  end
module PutBucketAclRequest =
  struct
    type t =
      {
      a_c_l: BucketCannedACL.t option ;
      access_control_policy: AccessControlPolicy.t option ;
      bucket: String.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      grant_full_control: String.t option ;
      grant_read: String.t option ;
      grant_read_a_c_p: String.t option ;
      grant_write: String.t option ;
      grant_write_a_c_p: String.t option ;
      expected_bucket_owner: String.t option }
    let make ?a_c_l  ?access_control_policy  ~bucket  ?content_m_d5 
      ?checksum_algorithm  ?grant_full_control  ?grant_read 
      ?grant_read_a_c_p  ?grant_write  ?grant_write_a_c_p 
      ?expected_bucket_owner  () =
      {
        a_c_l;
        access_control_policy;
        bucket;
        content_m_d5;
        checksum_algorithm;
        grant_full_control;
        grant_read;
        grant_read_a_c_p;
        grant_write;
        grant_write_a_c_p;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          a_c_l =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-acl" xml)
               BucketCannedACL.parse);
          access_control_policy =
            (Aws.Util.option_bind (Aws.Xml.member "AccessControlPolicy" xml)
               AccessControlPolicy.parse);
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          grant_full_control =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-grant-full-control" xml) String.parse);
          grant_read =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-read" xml)
               String.parse);
          grant_read_a_c_p =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-read-acp" xml)
               String.parse);
          grant_write =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-write" xml)
               String.parse);
          grant_write_a_c_p =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-grant-write-acp" xml) String.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.grant_write_a_c_p
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-write-acp", (String.to_query f)));
           Aws.Util.option_map v.grant_write
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-write", (String.to_query f)));
           Aws.Util.option_map v.grant_read_a_c_p
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-read-acp", (String.to_query f)));
           Aws.Util.option_map v.grant_read
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-read", (String.to_query f)));
           Aws.Util.option_map v.grant_full_control
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-grant-full-control", (String.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)));
           Aws.Util.option_map v.access_control_policy
             (fun f ->
                Aws.Query.Pair
                  ("AccessControlPolicy", (AccessControlPolicy.to_query f)));
           Aws.Util.option_map v.a_c_l
             (fun f ->
                Aws.Query.Pair ("x-amz-acl", (BucketCannedACL.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.grant_write_a_c_p
             (fun f -> ("x-amz-grant-write-acp", (String.to_json f)));
           Aws.Util.option_map v.grant_write
             (fun f -> ("x-amz-grant-write", (String.to_json f)));
           Aws.Util.option_map v.grant_read_a_c_p
             (fun f -> ("x-amz-grant-read-acp", (String.to_json f)));
           Aws.Util.option_map v.grant_read
             (fun f -> ("x-amz-grant-read", (String.to_json f)));
           Aws.Util.option_map v.grant_full_control
             (fun f -> ("x-amz-grant-full-control", (String.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket));
           Aws.Util.option_map v.access_control_policy
             (fun f ->
                ("AccessControlPolicy", (AccessControlPolicy.to_json f)));
           Aws.Util.option_map v.a_c_l
             (fun f -> ("x-amz-acl", (BucketCannedACL.to_json f)))])
    let of_json j =
      {
        a_c_l =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-acl")
             BucketCannedACL.of_json);
        access_control_policy =
          (Aws.Util.option_map (Aws.Json.lookup j "AccessControlPolicy")
             AccessControlPolicy.of_json);
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        grant_full_control =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-full-control")
             String.of_json);
        grant_read =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-read")
             String.of_json);
        grant_read_a_c_p =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-read-acp")
             String.of_json);
        grant_write =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-write")
             String.of_json);
        grant_write_a_c_p =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-write-acp")
             String.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module CloudFunctionConfiguration =
  struct
    type t =
      {
      id: String.t option ;
      event: Event.t option ;
      events: EventList.t ;
      cloud_function: String.t option ;
      invocation_role: String.t option }
    let make ?id  ?event  ?(events= [])  ?cloud_function  ?invocation_role 
      () = { id; event; events; cloud_function; invocation_role }
    let parse xml =
      Some
        {
          id = (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse);
          event =
            (Aws.Util.option_bind (Aws.Xml.member "Event" xml) Event.parse);
          events = (Aws.Util.of_option [] (EventList.parse xml));
          cloud_function =
            (Aws.Util.option_bind (Aws.Xml.member "CloudFunction" xml)
               String.parse);
          invocation_role =
            (Aws.Util.option_bind (Aws.Xml.member "InvocationRole" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.invocation_role
              (fun f ->
                 Aws.Query.Pair ("InvocationRole", (String.to_query f)));
           Aws.Util.option_map v.cloud_function
             (fun f -> Aws.Query.Pair ("CloudFunction", (String.to_query f)));
           Some (Aws.Query.Pair ("Event", (EventList.to_query v.events)));
           Aws.Util.option_map v.event
             (fun f -> Aws.Query.Pair ("Event", (Event.to_query f)));
           Aws.Util.option_map v.id
             (fun f -> Aws.Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.invocation_role
              (fun f -> ("InvocationRole", (String.to_json f)));
           Aws.Util.option_map v.cloud_function
             (fun f -> ("CloudFunction", (String.to_json f)));
           Some ("Event", (EventList.to_json v.events));
           Aws.Util.option_map v.event
             (fun f -> ("Event", (Event.to_json f)));
           Aws.Util.option_map v.id (fun f -> ("Id", (String.to_json f)))])
    let of_json j =
      {
        id = (Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json);
        event =
          (Aws.Util.option_map (Aws.Json.lookup j "Event") Event.of_json);
        events =
          (EventList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Event")));
        cloud_function =
          (Aws.Util.option_map (Aws.Json.lookup j "CloudFunction")
             String.of_json);
        invocation_role =
          (Aws.Util.option_map (Aws.Json.lookup j "InvocationRole")
             String.of_json)
      }
  end
module PutObjectRequest =
  struct
    type t =
      {
      a_c_l: ObjectCannedACL.t option ;
      body: Blob.t option ;
      bucket: String.t ;
      cache_control: String.t option ;
      content_disposition: String.t option ;
      content_encoding: String.t option ;
      content_language: String.t option ;
      content_length: Long.t option ;
      content_m_d5: String.t option ;
      content_type: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option ;
      expires: DateTime.t option ;
      grant_full_control: String.t option ;
      grant_read: String.t option ;
      grant_read_a_c_p: String.t option ;
      grant_write_a_c_p: String.t option ;
      key: String.t ;
      metadata: Metadata.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      storage_class: StorageClass.t option ;
      website_redirect_location: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      s_s_e_k_m_s_encryption_context: String.t option ;
      bucket_key_enabled: Boolean.t option ;
      request_payer: RequestPayer.t option ;
      tagging: String.t option ;
      object_lock_mode: ObjectLockMode.t option ;
      object_lock_retain_until_date: DateTime.t option ;
      object_lock_legal_hold_status: ObjectLockLegalHoldStatus.t option ;
      expected_bucket_owner: String.t option }
    let make ?a_c_l  ?body  ~bucket  ?cache_control  ?content_disposition 
      ?content_encoding  ?content_language  ?content_length  ?content_m_d5 
      ?content_type  ?checksum_algorithm  ?checksum_c_r_c32 
      ?checksum_c_r_c32_c  ?checksum_s_h_a1  ?checksum_s_h_a256  ?expires 
      ?grant_full_control  ?grant_read  ?grant_read_a_c_p  ?grant_write_a_c_p
       ~key  ?metadata  ?server_side_encryption  ?storage_class 
      ?website_redirect_location  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key  ?s_s_e_customer_key_m_d5  ?s_s_e_k_m_s_key_id 
      ?s_s_e_k_m_s_encryption_context  ?bucket_key_enabled  ?request_payer 
      ?tagging  ?object_lock_mode  ?object_lock_retain_until_date 
      ?object_lock_legal_hold_status  ?expected_bucket_owner  () =
      {
        a_c_l;
        body;
        bucket;
        cache_control;
        content_disposition;
        content_encoding;
        content_language;
        content_length;
        content_m_d5;
        content_type;
        checksum_algorithm;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256;
        expires;
        grant_full_control;
        grant_read;
        grant_read_a_c_p;
        grant_write_a_c_p;
        key;
        metadata;
        server_side_encryption;
        storage_class;
        website_redirect_location;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        s_s_e_k_m_s_encryption_context;
        bucket_key_enabled;
        request_payer;
        tagging;
        object_lock_mode;
        object_lock_retain_until_date;
        object_lock_legal_hold_status;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          a_c_l =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-acl" xml)
               ObjectCannedACL.parse);
          body =
            (Aws.Util.option_bind (Aws.Xml.member "Body" xml) Blob.parse);
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          cache_control =
            (Aws.Util.option_bind (Aws.Xml.member "Cache-Control" xml)
               String.parse);
          content_disposition =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Disposition" xml)
               String.parse);
          content_encoding =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Encoding" xml)
               String.parse);
          content_language =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Language" xml)
               String.parse);
          content_length =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Length" xml)
               Long.parse);
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          content_type =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Type" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-crc32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-crc32c" xml) String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-sha1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-sha256" xml) String.parse);
          expires =
            (Aws.Util.option_bind (Aws.Xml.member "Expires" xml)
               DateTime.parse);
          grant_full_control =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-grant-full-control" xml) String.parse);
          grant_read =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-read" xml)
               String.parse);
          grant_read_a_c_p =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-read-acp" xml)
               String.parse);
          grant_write_a_c_p =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-grant-write-acp" xml) String.parse);
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          metadata =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-meta-" xml)
               Metadata.parse);
          server_side_encryption =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-storage-class" xml)
               StorageClass.parse);
          website_redirect_location =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-website-redirect-location" xml)
               String.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-customer-key"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          s_s_e_k_m_s_key_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-aws-kms-key-id"
                  xml) String.parse);
          s_s_e_k_m_s_encryption_context =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-context" xml)
               String.parse);
          bucket_key_enabled =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-bucket-key-enabled" xml)
               Boolean.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          tagging =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-tagging" xml)
               String.parse);
          object_lock_mode =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-mode" xml)
               ObjectLockMode.parse);
          object_lock_retain_until_date =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-retain-until-date" xml)
               DateTime.parse);
          object_lock_legal_hold_status =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-legal-hold" xml)
               ObjectLockLegalHoldStatus.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.object_lock_legal_hold_status
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-object-lock-legal-hold",
                    (ObjectLockLegalHoldStatus.to_query f)));
           Aws.Util.option_map v.object_lock_retain_until_date
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-object-lock-retain-until-date",
                    (DateTime.to_query f)));
           Aws.Util.option_map v.object_lock_mode
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-object-lock-mode", (ObjectLockMode.to_query f)));
           Aws.Util.option_map v.tagging
             (fun f -> Aws.Query.Pair ("x-amz-tagging", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-bucket-key-enabled",
                    (Boolean.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_encryption_context
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-context",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.website_redirect_location
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-website-redirect-location", (String.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-storage-class", (StorageClass.to_query f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Aws.Util.option_map v.metadata
             (fun f -> Aws.Query.Pair ("x-amz-meta-", (Metadata.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Aws.Util.option_map v.grant_write_a_c_p
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-write-acp", (String.to_query f)));
           Aws.Util.option_map v.grant_read_a_c_p
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-read-acp", (String.to_query f)));
           Aws.Util.option_map v.grant_read
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-read", (String.to_query f)));
           Aws.Util.option_map v.grant_full_control
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-grant-full-control", (String.to_query f)));
           Aws.Util.option_map v.expires
             (fun f -> Aws.Query.Pair ("Expires", (DateTime.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32c", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32", (String.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_type
             (fun f -> Aws.Query.Pair ("Content-Type", (String.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Aws.Util.option_map v.content_length
             (fun f -> Aws.Query.Pair ("Content-Length", (Long.to_query f)));
           Aws.Util.option_map v.content_language
             (fun f ->
                Aws.Query.Pair ("Content-Language", (String.to_query f)));
           Aws.Util.option_map v.content_encoding
             (fun f ->
                Aws.Query.Pair ("Content-Encoding", (String.to_query f)));
           Aws.Util.option_map v.content_disposition
             (fun f ->
                Aws.Query.Pair ("Content-Disposition", (String.to_query f)));
           Aws.Util.option_map v.cache_control
             (fun f -> Aws.Query.Pair ("Cache-Control", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)));
           Aws.Util.option_map v.body
             (fun f -> Aws.Query.Pair ("Body", (Blob.to_query f)));
           Aws.Util.option_map v.a_c_l
             (fun f ->
                Aws.Query.Pair ("x-amz-acl", (ObjectCannedACL.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.object_lock_legal_hold_status
             (fun f ->
                ("x-amz-object-lock-legal-hold",
                  (ObjectLockLegalHoldStatus.to_json f)));
           Aws.Util.option_map v.object_lock_retain_until_date
             (fun f ->
                ("x-amz-object-lock-retain-until-date", (DateTime.to_json f)));
           Aws.Util.option_map v.object_lock_mode
             (fun f -> ("x-amz-object-lock-mode", (ObjectLockMode.to_json f)));
           Aws.Util.option_map v.tagging
             (fun f -> ("x-amz-tagging", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                ("x-amz-server-side-encryption-bucket-key-enabled",
                  (Boolean.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_encryption_context
             (fun f ->
                ("x-amz-server-side-encryption-context", (String.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                ("x-amz-server-side-encryption-aws-kms-key-id",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                ("x-amz-server-side-encryption-customer-key",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.website_redirect_location
             (fun f ->
                ("x-amz-website-redirect-location", (String.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f -> ("x-amz-storage-class", (StorageClass.to_json f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                ("x-amz-server-side-encryption",
                  (ServerSideEncryption.to_json f)));
           Aws.Util.option_map v.metadata
             (fun f -> ("x-amz-meta-", (Metadata.to_json f)));
           Some ("Key", (String.to_json v.key));
           Aws.Util.option_map v.grant_write_a_c_p
             (fun f -> ("x-amz-grant-write-acp", (String.to_json f)));
           Aws.Util.option_map v.grant_read_a_c_p
             (fun f -> ("x-amz-grant-read-acp", (String.to_json f)));
           Aws.Util.option_map v.grant_read
             (fun f -> ("x-amz-grant-read", (String.to_json f)));
           Aws.Util.option_map v.grant_full_control
             (fun f -> ("x-amz-grant-full-control", (String.to_json f)));
           Aws.Util.option_map v.expires
             (fun f -> ("Expires", (DateTime.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f -> ("x-amz-checksum-sha256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("x-amz-checksum-sha1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("x-amz-checksum-crc32c", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("x-amz-checksum-crc32", (String.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_type
             (fun f -> ("Content-Type", (String.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Aws.Util.option_map v.content_length
             (fun f -> ("Content-Length", (Long.to_json f)));
           Aws.Util.option_map v.content_language
             (fun f -> ("Content-Language", (String.to_json f)));
           Aws.Util.option_map v.content_encoding
             (fun f -> ("Content-Encoding", (String.to_json f)));
           Aws.Util.option_map v.content_disposition
             (fun f -> ("Content-Disposition", (String.to_json f)));
           Aws.Util.option_map v.cache_control
             (fun f -> ("Cache-Control", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket));
           Aws.Util.option_map v.body (fun f -> ("Body", (Blob.to_json f)));
           Aws.Util.option_map v.a_c_l
             (fun f -> ("x-amz-acl", (ObjectCannedACL.to_json f)))])
    let of_json j =
      {
        a_c_l =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-acl")
             ObjectCannedACL.of_json);
        body = (Aws.Util.option_map (Aws.Json.lookup j "Body") Blob.of_json);
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        cache_control =
          (Aws.Util.option_map (Aws.Json.lookup j "Cache-Control")
             String.of_json);
        content_disposition =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Disposition")
             String.of_json);
        content_encoding =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Encoding")
             String.of_json);
        content_language =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Language")
             String.of_json);
        content_length =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Length")
             Long.of_json);
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        content_type =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Type")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32c")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha256")
             String.of_json);
        expires =
          (Aws.Util.option_map (Aws.Json.lookup j "Expires") DateTime.of_json);
        grant_full_control =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-full-control")
             String.of_json);
        grant_read =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-read")
             String.of_json);
        grant_read_a_c_p =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-read-acp")
             String.of_json);
        grant_write_a_c_p =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-write-acp")
             String.of_json);
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        metadata =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-meta-")
             Metadata.of_json);
        server_side_encryption =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption")
             ServerSideEncryption.of_json);
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-storage-class")
             StorageClass.of_json);
        website_redirect_location =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-website-redirect-location")
             String.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-customer-key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-aws-kms-key-id")
             String.of_json);
        s_s_e_k_m_s_encryption_context =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-context")
             String.of_json);
        bucket_key_enabled =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-bucket-key-enabled")
             Boolean.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        tagging =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-tagging")
             String.of_json);
        object_lock_mode =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-object-lock-mode")
             ObjectLockMode.of_json);
        object_lock_retain_until_date =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-object-lock-retain-until-date")
             DateTime.of_json);
        object_lock_legal_hold_status =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-object-lock-legal-hold")
             ObjectLockLegalHoldStatus.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module ObjectIdentifier =
  struct
    type t = {
      key: String.t ;
      version_id: String.t option }
    let make ~key  ?version_id  () = { key; version_id }
    let parse xml =
      Some
        {
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "VersionId" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.version_id
              (fun f -> Aws.Query.Pair ("VersionId", (String.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.version_id
              (fun f -> ("VersionId", (String.to_json f)));
           Some ("Key", (String.to_json v.key))])
    let of_json j =
      {
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "VersionId") String.of_json)
      }
  end
module ListObjectsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      delimiter: String.t option ;
      encoding_type: EncodingType.t option ;
      marker: String.t option ;
      max_keys: Integer.t option ;
      prefix: String.t option ;
      request_payer: RequestPayer.t option ;
      expected_bucket_owner: String.t option ;
      optional_object_attributes: OptionalObjectAttributesList.t }
    let make ~bucket  ?delimiter  ?encoding_type  ?marker  ?max_keys  ?prefix
       ?request_payer  ?expected_bucket_owner  ?(optional_object_attributes=
      [])  () =
      {
        bucket;
        delimiter;
        encoding_type;
        marker;
        max_keys;
        prefix;
        request_payer;
        expected_bucket_owner;
        optional_object_attributes
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          delimiter =
            (Aws.Util.option_bind (Aws.Xml.member "delimiter" xml)
               String.parse);
          encoding_type =
            (Aws.Util.option_bind (Aws.Xml.member "encoding-type" xml)
               EncodingType.parse);
          marker =
            (Aws.Util.option_bind (Aws.Xml.member "marker" xml) String.parse);
          max_keys =
            (Aws.Util.option_bind (Aws.Xml.member "max-keys" xml)
               Integer.parse);
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "prefix" xml) String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          optional_object_attributes =
            (Aws.Util.of_option []
               (Aws.Util.option_bind
                  (Aws.Xml.member "x-amz-optional-object-attributes" xml)
                  OptionalObjectAttributesList.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("x-amz-optional-object-attributes",
                   (OptionalObjectAttributesList.to_query
                      v.optional_object_attributes)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("prefix", (String.to_query f)));
           Aws.Util.option_map v.max_keys
             (fun f -> Aws.Query.Pair ("max-keys", (Integer.to_query f)));
           Aws.Util.option_map v.marker
             (fun f -> Aws.Query.Pair ("marker", (String.to_query f)));
           Aws.Util.option_map v.encoding_type
             (fun f ->
                Aws.Query.Pair ("encoding-type", (EncodingType.to_query f)));
           Aws.Util.option_map v.delimiter
             (fun f -> Aws.Query.Pair ("delimiter", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("x-amz-optional-object-attributes",
                (OptionalObjectAttributesList.to_json
                   v.optional_object_attributes));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("prefix", (String.to_json f)));
           Aws.Util.option_map v.max_keys
             (fun f -> ("max-keys", (Integer.to_json f)));
           Aws.Util.option_map v.marker
             (fun f -> ("marker", (String.to_json f)));
           Aws.Util.option_map v.encoding_type
             (fun f -> ("encoding-type", (EncodingType.to_json f)));
           Aws.Util.option_map v.delimiter
             (fun f -> ("delimiter", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        delimiter =
          (Aws.Util.option_map (Aws.Json.lookup j "delimiter") String.of_json);
        encoding_type =
          (Aws.Util.option_map (Aws.Json.lookup j "encoding-type")
             EncodingType.of_json);
        marker =
          (Aws.Util.option_map (Aws.Json.lookup j "marker") String.of_json);
        max_keys =
          (Aws.Util.option_map (Aws.Json.lookup j "max-keys") Integer.of_json);
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "prefix") String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        optional_object_attributes =
          (OptionalObjectAttributesList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "x-amz-optional-object-attributes")))
      }
  end
module GetBucketAnalyticsConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      id: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~id  ?expected_bucket_owner  () =
      { bucket; id; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          id =
            (Aws.Xml.required "id"
               (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("id", (String.to_query v.id)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("id", (String.to_json v.id));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module CopyPartResult =
  struct
    type t =
      {
      e_tag: String.t option ;
      last_modified: DateTime.t option ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option }
    let make ?e_tag  ?last_modified  ?checksum_c_r_c32  ?checksum_c_r_c32_c 
      ?checksum_s_h_a1  ?checksum_s_h_a256  () =
      {
        e_tag;
        last_modified;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256
      }
    let parse xml =
      Some
        {
          e_tag =
            (Aws.Util.option_bind (Aws.Xml.member "ETag" xml) String.parse);
          last_modified =
            (Aws.Util.option_bind (Aws.Xml.member "LastModified" xml)
               DateTime.parse);
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32C" xml)
               String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA256" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_s_h_a256
              (fun f ->
                 Aws.Query.Pair ("ChecksumSHA256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> Aws.Query.Pair ("ChecksumSHA1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> Aws.Query.Pair ("ChecksumCRC32C", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> Aws.Query.Pair ("ChecksumCRC32", (String.to_query f)));
           Aws.Util.option_map v.last_modified
             (fun f -> Aws.Query.Pair ("LastModified", (DateTime.to_query f)));
           Aws.Util.option_map v.e_tag
             (fun f -> Aws.Query.Pair ("ETag", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_s_h_a256
              (fun f -> ("ChecksumSHA256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("ChecksumSHA1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("ChecksumCRC32C", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("ChecksumCRC32", (String.to_json f)));
           Aws.Util.option_map v.last_modified
             (fun f -> ("LastModified", (DateTime.to_json f)));
           Aws.Util.option_map v.e_tag
             (fun f -> ("ETag", (String.to_json f)))])
    let of_json j =
      {
        e_tag =
          (Aws.Util.option_map (Aws.Json.lookup j "ETag") String.of_json);
        last_modified =
          (Aws.Util.option_map (Aws.Json.lookup j "LastModified")
             DateTime.of_json);
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32C")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA256")
             String.of_json)
      }
  end
module UploadPartCopyOutput =
  struct
    type t =
      {
      copy_source_version_id: String.t option ;
      copy_part_result: CopyPartResult.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      bucket_key_enabled: Boolean.t option ;
      request_charged: RequestCharged.t option }
    let make ?copy_source_version_id  ?copy_part_result 
      ?server_side_encryption  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key_m_d5  ?s_s_e_k_m_s_key_id  ?bucket_key_enabled 
      ?request_charged  () =
      {
        copy_source_version_id;
        copy_part_result;
        server_side_encryption;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        bucket_key_enabled;
        request_charged
      }
    let parse xml =
      Some
        {
          copy_source_version_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-copy-source-version-id" xml)
               String.parse);
          copy_part_result =
            (Aws.Util.option_bind (Aws.Xml.member "CopyPartResult" xml)
               CopyPartResult.parse);
          server_side_encryption =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          s_s_e_k_m_s_key_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-aws-kms-key-id"
                  xml) String.parse);
          bucket_key_enabled =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-bucket-key-enabled" xml)
               Boolean.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-bucket-key-enabled",
                    (Boolean.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Aws.Util.option_map v.copy_part_result
             (fun f ->
                Aws.Query.Pair
                  ("CopyPartResult", (CopyPartResult.to_query f)));
           Aws.Util.option_map v.copy_source_version_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-version-id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                ("x-amz-server-side-encryption-bucket-key-enabled",
                  (Boolean.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                ("x-amz-server-side-encryption-aws-kms-key-id",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                ("x-amz-server-side-encryption",
                  (ServerSideEncryption.to_json f)));
           Aws.Util.option_map v.copy_part_result
             (fun f -> ("CopyPartResult", (CopyPartResult.to_json f)));
           Aws.Util.option_map v.copy_source_version_id
             (fun f -> ("x-amz-copy-source-version-id", (String.to_json f)))])
    let of_json j =
      {
        copy_source_version_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-copy-source-version-id")
             String.of_json);
        copy_part_result =
          (Aws.Util.option_map (Aws.Json.lookup j "CopyPartResult")
             CopyPartResult.of_json);
        server_side_encryption =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption")
             ServerSideEncryption.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-aws-kms-key-id")
             String.of_json);
        bucket_key_enabled =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-bucket-key-enabled")
             Boolean.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module StorageClassAnalysisSchemaVersion =
  struct
    type t =
      | V_1 
    let str_to_t = [("V_1", V_1)]
    let t_to_str = [(V_1, "V_1")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module AnalyticsS3BucketDestination =
  struct
    type t =
      {
      format: AnalyticsS3ExportFileFormat.t ;
      bucket_account_id: String.t option ;
      bucket: String.t ;
      prefix: String.t option }
    let make ~format  ?bucket_account_id  ~bucket  ?prefix  () =
      { format; bucket_account_id; bucket; prefix }
    let parse xml =
      Some
        {
          format =
            (Aws.Xml.required "Format"
               (Aws.Util.option_bind (Aws.Xml.member "Format" xml)
                  AnalyticsS3ExportFileFormat.parse));
          bucket_account_id =
            (Aws.Util.option_bind (Aws.Xml.member "BucketAccountId" xml)
               String.parse);
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.prefix
              (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)));
           Aws.Util.option_map v.bucket_account_id
             (fun f ->
                Aws.Query.Pair ("BucketAccountId", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("Format", (AnalyticsS3ExportFileFormat.to_query v.format)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.prefix
              (fun f -> ("Prefix", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket));
           Aws.Util.option_map v.bucket_account_id
             (fun f -> ("BucketAccountId", (String.to_json f)));
           Some ("Format", (AnalyticsS3ExportFileFormat.to_json v.format))])
    let of_json j =
      {
        format =
          (AnalyticsS3ExportFileFormat.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Format")));
        bucket_account_id =
          (Aws.Util.option_map (Aws.Json.lookup j "BucketAccountId")
             String.of_json);
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json)
      }
  end
module AnalyticsExportDestination =
  struct
    type t = {
      s3_bucket_destination: AnalyticsS3BucketDestination.t }
    let make ~s3_bucket_destination  () = { s3_bucket_destination }
    let parse xml =
      Some
        {
          s3_bucket_destination =
            (Aws.Xml.required "S3BucketDestination"
               (Aws.Util.option_bind
                  (Aws.Xml.member "S3BucketDestination" xml)
                  AnalyticsS3BucketDestination.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("S3BucketDestination",
                   (AnalyticsS3BucketDestination.to_query
                      v.s3_bucket_destination)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("S3BucketDestination",
                (AnalyticsS3BucketDestination.to_json v.s3_bucket_destination))])
    let of_json j =
      {
        s3_bucket_destination =
          (AnalyticsS3BucketDestination.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "S3BucketDestination")))
      }
  end
module StorageClassAnalysisDataExport =
  struct
    type t =
      {
      output_schema_version: StorageClassAnalysisSchemaVersion.t ;
      destination: AnalyticsExportDestination.t }
    let make ~output_schema_version  ~destination  () =
      { output_schema_version; destination }
    let parse xml =
      Some
        {
          output_schema_version =
            (Aws.Xml.required "OutputSchemaVersion"
               (Aws.Util.option_bind
                  (Aws.Xml.member "OutputSchemaVersion" xml)
                  StorageClassAnalysisSchemaVersion.parse));
          destination =
            (Aws.Xml.required "Destination"
               (Aws.Util.option_bind (Aws.Xml.member "Destination" xml)
                  AnalyticsExportDestination.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("Destination",
                   (AnalyticsExportDestination.to_query v.destination)));
           Some
             (Aws.Query.Pair
                ("OutputSchemaVersion",
                  (StorageClassAnalysisSchemaVersion.to_query
                     v.output_schema_version)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("Destination",
                (AnalyticsExportDestination.to_json v.destination));
           Some
             ("OutputSchemaVersion",
               (StorageClassAnalysisSchemaVersion.to_json
                  v.output_schema_version))])
    let of_json j =
      {
        output_schema_version =
          (StorageClassAnalysisSchemaVersion.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "OutputSchemaVersion")));
        destination =
          (AnalyticsExportDestination.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Destination")))
      }
  end
module ReplicationTimeValue =
  struct
    type t = {
      minutes: Integer.t option }
    let make ?minutes  () = { minutes }
    let parse xml =
      Some
        {
          minutes =
            (Aws.Util.option_bind (Aws.Xml.member "Minutes" xml)
               Integer.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.minutes
              (fun f -> Aws.Query.Pair ("Minutes", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.minutes
              (fun f -> ("Minutes", (Integer.to_json f)))])
    let of_json j =
      {
        minutes =
          (Aws.Util.option_map (Aws.Json.lookup j "Minutes") Integer.of_json)
      }
  end
module ReplicationTimeStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)]
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ReplicationTime =
  struct
    type t = {
      status: ReplicationTimeStatus.t ;
      time: ReplicationTimeValue.t }
    let make ~status  ~time  () = { status; time }
    let parse xml =
      Some
        {
          status =
            (Aws.Xml.required "Status"
               (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
                  ReplicationTimeStatus.parse));
          time =
            (Aws.Xml.required "Time"
               (Aws.Util.option_bind (Aws.Xml.member "Time" xml)
                  ReplicationTimeValue.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("Time", (ReplicationTimeValue.to_query v.time)));
           Some
             (Aws.Query.Pair
                ("Status", (ReplicationTimeStatus.to_query v.status)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Time", (ReplicationTimeValue.to_json v.time));
           Some ("Status", (ReplicationTimeStatus.to_json v.status))])
    let of_json j =
      {
        status =
          (ReplicationTimeStatus.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Status")));
        time =
          (ReplicationTimeValue.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Time")))
      }
  end
module PutBucketInventoryConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      id: String.t ;
      inventory_configuration: InventoryConfiguration.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~id  ~inventory_configuration  ?expected_bucket_owner 
      () = { bucket; id; inventory_configuration; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          id =
            (Aws.Xml.required "id"
               (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse));
          inventory_configuration =
            (Aws.Xml.required "InventoryConfiguration"
               (Aws.Util.option_bind
                  (Aws.Xml.member "InventoryConfiguration" xml)
                  InventoryConfiguration.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("InventoryConfiguration",
                  (InventoryConfiguration.to_query v.inventory_configuration)));
           Some (Aws.Query.Pair ("id", (String.to_query v.id)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some
             ("InventoryConfiguration",
               (InventoryConfiguration.to_json v.inventory_configuration));
           Some ("id", (String.to_json v.id));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id")));
        inventory_configuration =
          (InventoryConfiguration.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "InventoryConfiguration")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module Error =
  struct
    type t =
      {
      key: String.t option ;
      version_id: String.t option ;
      code: String.t option ;
      message: String.t option }
    let make ?key  ?version_id  ?code  ?message  () =
      { key; version_id; code; message }
    let parse xml =
      Some
        {
          key =
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "VersionId" xml)
               String.parse);
          code =
            (Aws.Util.option_bind (Aws.Xml.member "Code" xml) String.parse);
          message =
            (Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> Aws.Query.Pair ("Message", (String.to_query f)));
           Aws.Util.option_map v.code
             (fun f -> Aws.Query.Pair ("Code", (String.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("VersionId", (String.to_query f)));
           Aws.Util.option_map v.key
             (fun f -> Aws.Query.Pair ("Key", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.message
              (fun f -> ("Message", (String.to_json f)));
           Aws.Util.option_map v.code (fun f -> ("Code", (String.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("VersionId", (String.to_json f)));
           Aws.Util.option_map v.key (fun f -> ("Key", (String.to_json f)))])
    let of_json j =
      {
        key = (Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "VersionId") String.of_json);
        code =
          (Aws.Util.option_map (Aws.Json.lookup j "Code") String.of_json);
        message =
          (Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json)
      }
  end
module Errors =
  struct
    type t = Error.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map Error.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list Error.to_query v
    let to_json v = `List (List.map Error.to_json v)
    let of_json j = Aws.Json.to_list Error.of_json j
  end
module DeletedObjects =
  struct
    type t = DeletedObject.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map DeletedObject.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list DeletedObject.to_query v
    let to_json v = `List (List.map DeletedObject.to_json v)
    let of_json j = Aws.Json.to_list DeletedObject.of_json j
  end
module DeleteObjectsOutput =
  struct
    type t =
      {
      deleted: DeletedObjects.t ;
      request_charged: RequestCharged.t option ;
      errors: Errors.t }
    let make ?(deleted= [])  ?request_charged  ?(errors= [])  () =
      { deleted; request_charged; errors }
    let parse xml =
      Some
        {
          deleted = (Aws.Util.of_option [] (DeletedObjects.parse xml));
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse);
          errors = (Aws.Util.of_option [] (Errors.parse xml))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Error", (Errors.to_query v.errors)));
           Aws.Util.option_map v.request_charged
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-charged", (RequestCharged.to_query f)));
           Some
             (Aws.Query.Pair
                ("Deleted.member", (DeletedObjects.to_query v.deleted)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Error", (Errors.to_json v.errors));
           Aws.Util.option_map v.request_charged
             (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Some ("Deleted", (DeletedObjects.to_json v.deleted))])
    let of_json j =
      {
        deleted =
          (DeletedObjects.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Deleted")));
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json);
        errors =
          (Errors.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Error")))
      }
  end
module PutBucketIntelligentTieringConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      id: String.t ;
      intelligent_tiering_configuration: IntelligentTieringConfiguration.t }
    let make ~bucket  ~id  ~intelligent_tiering_configuration  () =
      { bucket; id; intelligent_tiering_configuration }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          id =
            (Aws.Xml.required "id"
               (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse));
          intelligent_tiering_configuration =
            (Aws.Xml.required "IntelligentTieringConfiguration"
               (Aws.Util.option_bind
                  (Aws.Xml.member "IntelligentTieringConfiguration" xml)
                  IntelligentTieringConfiguration.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("IntelligentTieringConfiguration",
                   (IntelligentTieringConfiguration.to_query
                      v.intelligent_tiering_configuration)));
           Some (Aws.Query.Pair ("id", (String.to_query v.id)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("IntelligentTieringConfiguration",
                (IntelligentTieringConfiguration.to_json
                   v.intelligent_tiering_configuration));
           Some ("id", (String.to_json v.id));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id")));
        intelligent_tiering_configuration =
          (IntelligentTieringConfiguration.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "IntelligentTieringConfiguration")))
      }
  end
module ListObjectsV2Request =
  struct
    type t =
      {
      bucket: String.t ;
      delimiter: String.t option ;
      encoding_type: EncodingType.t option ;
      max_keys: Integer.t option ;
      prefix: String.t option ;
      continuation_token: String.t option ;
      fetch_owner: Boolean.t option ;
      start_after: String.t option ;
      request_payer: RequestPayer.t option ;
      expected_bucket_owner: String.t option ;
      optional_object_attributes: OptionalObjectAttributesList.t }
    let make ~bucket  ?delimiter  ?encoding_type  ?max_keys  ?prefix 
      ?continuation_token  ?fetch_owner  ?start_after  ?request_payer 
      ?expected_bucket_owner  ?(optional_object_attributes= [])  () =
      {
        bucket;
        delimiter;
        encoding_type;
        max_keys;
        prefix;
        continuation_token;
        fetch_owner;
        start_after;
        request_payer;
        expected_bucket_owner;
        optional_object_attributes
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          delimiter =
            (Aws.Util.option_bind (Aws.Xml.member "delimiter" xml)
               String.parse);
          encoding_type =
            (Aws.Util.option_bind (Aws.Xml.member "encoding-type" xml)
               EncodingType.parse);
          max_keys =
            (Aws.Util.option_bind (Aws.Xml.member "max-keys" xml)
               Integer.parse);
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "prefix" xml) String.parse);
          continuation_token =
            (Aws.Util.option_bind (Aws.Xml.member "continuation-token" xml)
               String.parse);
          fetch_owner =
            (Aws.Util.option_bind (Aws.Xml.member "fetch-owner" xml)
               Boolean.parse);
          start_after =
            (Aws.Util.option_bind (Aws.Xml.member "start-after" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          optional_object_attributes =
            (Aws.Util.of_option []
               (Aws.Util.option_bind
                  (Aws.Xml.member "x-amz-optional-object-attributes" xml)
                  OptionalObjectAttributesList.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("x-amz-optional-object-attributes",
                   (OptionalObjectAttributesList.to_query
                      v.optional_object_attributes)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.start_after
             (fun f -> Aws.Query.Pair ("start-after", (String.to_query f)));
           Aws.Util.option_map v.fetch_owner
             (fun f -> Aws.Query.Pair ("fetch-owner", (Boolean.to_query f)));
           Aws.Util.option_map v.continuation_token
             (fun f ->
                Aws.Query.Pair ("continuation-token", (String.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("prefix", (String.to_query f)));
           Aws.Util.option_map v.max_keys
             (fun f -> Aws.Query.Pair ("max-keys", (Integer.to_query f)));
           Aws.Util.option_map v.encoding_type
             (fun f ->
                Aws.Query.Pair ("encoding-type", (EncodingType.to_query f)));
           Aws.Util.option_map v.delimiter
             (fun f -> Aws.Query.Pair ("delimiter", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("x-amz-optional-object-attributes",
                (OptionalObjectAttributesList.to_json
                   v.optional_object_attributes));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.start_after
             (fun f -> ("start-after", (String.to_json f)));
           Aws.Util.option_map v.fetch_owner
             (fun f -> ("fetch-owner", (Boolean.to_json f)));
           Aws.Util.option_map v.continuation_token
             (fun f -> ("continuation-token", (String.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("prefix", (String.to_json f)));
           Aws.Util.option_map v.max_keys
             (fun f -> ("max-keys", (Integer.to_json f)));
           Aws.Util.option_map v.encoding_type
             (fun f -> ("encoding-type", (EncodingType.to_json f)));
           Aws.Util.option_map v.delimiter
             (fun f -> ("delimiter", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        delimiter =
          (Aws.Util.option_map (Aws.Json.lookup j "delimiter") String.of_json);
        encoding_type =
          (Aws.Util.option_map (Aws.Json.lookup j "encoding-type")
             EncodingType.of_json);
        max_keys =
          (Aws.Util.option_map (Aws.Json.lookup j "max-keys") Integer.of_json);
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "prefix") String.of_json);
        continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "continuation-token")
             String.of_json);
        fetch_owner =
          (Aws.Util.option_map (Aws.Json.lookup j "fetch-owner")
             Boolean.of_json);
        start_after =
          (Aws.Util.option_map (Aws.Json.lookup j "start-after")
             String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        optional_object_attributes =
          (OptionalObjectAttributesList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "x-amz-optional-object-attributes")))
      }
  end
module ObjectOwnership =
  struct
    type t =
      | BucketOwnerPreferred 
      | ObjectWriter 
      | BucketOwnerEnforced 
    let str_to_t =
      [("BucketOwnerEnforced", BucketOwnerEnforced);
      ("ObjectWriter", ObjectWriter);
      ("BucketOwnerPreferred", BucketOwnerPreferred)]
    let t_to_str =
      [(BucketOwnerEnforced, "BucketOwnerEnforced");
      (ObjectWriter, "ObjectWriter");
      (BucketOwnerPreferred, "BucketOwnerPreferred")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module OwnershipControlsRule =
  struct
    type t = {
      object_ownership: ObjectOwnership.t }
    let make ~object_ownership  () = { object_ownership }
    let parse xml =
      Some
        {
          object_ownership =
            (Aws.Xml.required "ObjectOwnership"
               (Aws.Util.option_bind (Aws.Xml.member "ObjectOwnership" xml)
                  ObjectOwnership.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("ObjectOwnership",
                   (ObjectOwnership.to_query v.object_ownership)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("ObjectOwnership",
                (ObjectOwnership.to_json v.object_ownership))])
    let of_json j =
      {
        object_ownership =
          (ObjectOwnership.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "ObjectOwnership")))
      }
  end
module OwnershipControlsRules =
  struct
    type t = OwnershipControlsRule.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map OwnershipControlsRule.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list OwnershipControlsRule.to_query v
    let to_json v = `List (List.map OwnershipControlsRule.to_json v)
    let of_json j = Aws.Json.to_list OwnershipControlsRule.of_json j
  end
module OwnershipControls =
  struct
    type t = {
      rules: OwnershipControlsRules.t }
    let make ~rules  () = { rules }
    let parse xml =
      Some
        {
          rules =
            (Aws.Xml.required "Rule" (OwnershipControlsRules.parse xml))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("Rule", (OwnershipControlsRules.to_query v.rules)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Rule", (OwnershipControlsRules.to_json v.rules))])
    let of_json j =
      {
        rules =
          (OwnershipControlsRules.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Rule")))
      }
  end
module ReplicaModificationsStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)]
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module Tier =
  struct
    type t =
      | Standard 
      | Bulk 
      | Expedited 
    let str_to_t =
      [("Expedited", Expedited); ("Bulk", Bulk); ("Standard", Standard)]
    let t_to_str =
      [(Expedited, "Expedited"); (Bulk, "Bulk"); (Standard, "Standard")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module GlacierJobParameters =
  struct
    type t = {
      tier: Tier.t }
    let make ~tier  () = { tier }
    let parse xml =
      Some
        {
          tier =
            (Aws.Xml.required "Tier"
               (Aws.Util.option_bind (Aws.Xml.member "Tier" xml) Tier.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Tier", (Tier.to_query v.tier)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt [Some ("Tier", (Tier.to_json v.tier))])
    let of_json j =
      {
        tier =
          (Tier.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tier")))
      }
  end
module OwnerOverride =
  struct
    type t =
      | Destination 
    let str_to_t = [("Destination", Destination)]
    let t_to_str = [(Destination, "Destination")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module AccessControlTranslation =
  struct
    type t = {
      owner: OwnerOverride.t }
    let make ~owner  () = { owner }
    let parse xml =
      Some
        {
          owner =
            (Aws.Xml.required "Owner"
               (Aws.Util.option_bind (Aws.Xml.member "Owner" xml)
                  OwnerOverride.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Owner", (OwnerOverride.to_query v.owner)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Owner", (OwnerOverride.to_json v.owner))])
    let of_json j =
      {
        owner =
          (OwnerOverride.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Owner")))
      }
  end
module ServerSideEncryptionByDefault =
  struct
    type t =
      {
      s_s_e_algorithm: ServerSideEncryption.t ;
      k_m_s_master_key_i_d: String.t option }
    let make ~s_s_e_algorithm  ?k_m_s_master_key_i_d  () =
      { s_s_e_algorithm; k_m_s_master_key_i_d }
    let parse xml =
      Some
        {
          s_s_e_algorithm =
            (Aws.Xml.required "SSEAlgorithm"
               (Aws.Util.option_bind (Aws.Xml.member "SSEAlgorithm" xml)
                  ServerSideEncryption.parse));
          k_m_s_master_key_i_d =
            (Aws.Util.option_bind (Aws.Xml.member "KMSMasterKeyID" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.k_m_s_master_key_i_d
              (fun f ->
                 Aws.Query.Pair ("KMSMasterKeyID", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("SSEAlgorithm",
                  (ServerSideEncryption.to_query v.s_s_e_algorithm)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.k_m_s_master_key_i_d
              (fun f -> ("KMSMasterKeyID", (String.to_json f)));
           Some
             ("SSEAlgorithm",
               (ServerSideEncryption.to_json v.s_s_e_algorithm))])
    let of_json j =
      {
        s_s_e_algorithm =
          (ServerSideEncryption.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "SSEAlgorithm")));
        k_m_s_master_key_i_d =
          (Aws.Util.option_map (Aws.Json.lookup j "KMSMasterKeyID")
             String.of_json)
      }
  end
module GetBucketIntelligentTieringConfigurationOutput =
  struct
    type t =
      {
      intelligent_tiering_configuration:
        IntelligentTieringConfiguration.t option }
    let make ?intelligent_tiering_configuration  () =
      { intelligent_tiering_configuration }
    let parse xml =
      Some
        {
          intelligent_tiering_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "IntelligentTieringConfiguration" xml)
               IntelligentTieringConfiguration.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.intelligent_tiering_configuration
              (fun f ->
                 Aws.Query.Pair
                   ("IntelligentTieringConfiguration",
                     (IntelligentTieringConfiguration.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.intelligent_tiering_configuration
              (fun f ->
                 ("IntelligentTieringConfiguration",
                   (IntelligentTieringConfiguration.to_json f)))])
    let of_json j =
      {
        intelligent_tiering_configuration =
          (Aws.Util.option_map
             (Aws.Json.lookup j "IntelligentTieringConfiguration")
             IntelligentTieringConfiguration.of_json)
      }
  end
module ScanRange =
  struct
    type t = {
      start: Long.t option ;
      end_: Long.t option }
    let make ?start  ?end_  () = { start; end_ }
    let parse xml =
      Some
        {
          start =
            (Aws.Util.option_bind (Aws.Xml.member "Start" xml) Long.parse);
          end_ = (Aws.Util.option_bind (Aws.Xml.member "End" xml) Long.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.end_
              (fun f -> Aws.Query.Pair ("End", (Long.to_query f)));
           Aws.Util.option_map v.start
             (fun f -> Aws.Query.Pair ("Start", (Long.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.end_ (fun f -> ("End", (Long.to_json f)));
           Aws.Util.option_map v.start (fun f -> ("Start", (Long.to_json f)))])
    let of_json j =
      {
        start =
          (Aws.Util.option_map (Aws.Json.lookup j "Start") Long.of_json);
        end_ = (Aws.Util.option_map (Aws.Json.lookup j "End") Long.of_json)
      }
  end
module RequestProgress =
  struct
    type t = {
      enabled: Boolean.t option }
    let make ?enabled  () = { enabled }
    let parse xml =
      Some
        {
          enabled =
            (Aws.Util.option_bind (Aws.Xml.member "Enabled" xml)
               Boolean.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.enabled
              (fun f -> Aws.Query.Pair ("Enabled", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.enabled
              (fun f -> ("Enabled", (Boolean.to_json f)))])
    let of_json j =
      {
        enabled =
          (Aws.Util.option_map (Aws.Json.lookup j "Enabled") Boolean.of_json)
      }
  end
module ParquetInput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])
    let to_json v = `Assoc (Aws.Util.list_filter_opt [])
    let of_json j = ()
  end
module JSONInput =
  struct
    type t = {
      type_: JSONType.t option }
    let make ?type_  () = { type_ }
    let parse xml =
      Some
        {
          type_ =
            (Aws.Util.option_bind (Aws.Xml.member "Type" xml) JSONType.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.type_
              (fun f -> Aws.Query.Pair ("Type", (JSONType.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.type_
              (fun f -> ("Type", (JSONType.to_json f)))])
    let of_json j =
      {
        type_ =
          (Aws.Util.option_map (Aws.Json.lookup j "Type") JSONType.of_json)
      }
  end
module CompressionType =
  struct
    type t =
      | NONE 
      | GZIP 
      | BZIP2 
    let str_to_t = [("BZIP2", BZIP2); ("GZIP", GZIP); ("NONE", NONE)]
    let t_to_str = [(BZIP2, "BZIP2"); (GZIP, "GZIP"); (NONE, "NONE")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module FileHeaderInfo =
  struct
    type t =
      | USE 
      | IGNORE 
      | NONE 
    let str_to_t = [("NONE", NONE); ("IGNORE", IGNORE); ("USE", USE)]
    let t_to_str = [(NONE, "NONE"); (IGNORE, "IGNORE"); (USE, "USE")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module CSVInput =
  struct
    type t =
      {
      file_header_info: FileHeaderInfo.t option ;
      comments: String.t option ;
      quote_escape_character: String.t option ;
      record_delimiter: String.t option ;
      field_delimiter: String.t option ;
      quote_character: String.t option ;
      allow_quoted_record_delimiter: Boolean.t option }
    let make ?file_header_info  ?comments  ?quote_escape_character 
      ?record_delimiter  ?field_delimiter  ?quote_character 
      ?allow_quoted_record_delimiter  () =
      {
        file_header_info;
        comments;
        quote_escape_character;
        record_delimiter;
        field_delimiter;
        quote_character;
        allow_quoted_record_delimiter
      }
    let parse xml =
      Some
        {
          file_header_info =
            (Aws.Util.option_bind (Aws.Xml.member "FileHeaderInfo" xml)
               FileHeaderInfo.parse);
          comments =
            (Aws.Util.option_bind (Aws.Xml.member "Comments" xml)
               String.parse);
          quote_escape_character =
            (Aws.Util.option_bind (Aws.Xml.member "QuoteEscapeCharacter" xml)
               String.parse);
          record_delimiter =
            (Aws.Util.option_bind (Aws.Xml.member "RecordDelimiter" xml)
               String.parse);
          field_delimiter =
            (Aws.Util.option_bind (Aws.Xml.member "FieldDelimiter" xml)
               String.parse);
          quote_character =
            (Aws.Util.option_bind (Aws.Xml.member "QuoteCharacter" xml)
               String.parse);
          allow_quoted_record_delimiter =
            (Aws.Util.option_bind
               (Aws.Xml.member "AllowQuotedRecordDelimiter" xml)
               Boolean.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.allow_quoted_record_delimiter
              (fun f ->
                 Aws.Query.Pair
                   ("AllowQuotedRecordDelimiter", (Boolean.to_query f)));
           Aws.Util.option_map v.quote_character
             (fun f -> Aws.Query.Pair ("QuoteCharacter", (String.to_query f)));
           Aws.Util.option_map v.field_delimiter
             (fun f -> Aws.Query.Pair ("FieldDelimiter", (String.to_query f)));
           Aws.Util.option_map v.record_delimiter
             (fun f ->
                Aws.Query.Pair ("RecordDelimiter", (String.to_query f)));
           Aws.Util.option_map v.quote_escape_character
             (fun f ->
                Aws.Query.Pair ("QuoteEscapeCharacter", (String.to_query f)));
           Aws.Util.option_map v.comments
             (fun f -> Aws.Query.Pair ("Comments", (String.to_query f)));
           Aws.Util.option_map v.file_header_info
             (fun f ->
                Aws.Query.Pair
                  ("FileHeaderInfo", (FileHeaderInfo.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.allow_quoted_record_delimiter
              (fun f -> ("AllowQuotedRecordDelimiter", (Boolean.to_json f)));
           Aws.Util.option_map v.quote_character
             (fun f -> ("QuoteCharacter", (String.to_json f)));
           Aws.Util.option_map v.field_delimiter
             (fun f -> ("FieldDelimiter", (String.to_json f)));
           Aws.Util.option_map v.record_delimiter
             (fun f -> ("RecordDelimiter", (String.to_json f)));
           Aws.Util.option_map v.quote_escape_character
             (fun f -> ("QuoteEscapeCharacter", (String.to_json f)));
           Aws.Util.option_map v.comments
             (fun f -> ("Comments", (String.to_json f)));
           Aws.Util.option_map v.file_header_info
             (fun f -> ("FileHeaderInfo", (FileHeaderInfo.to_json f)))])
    let of_json j =
      {
        file_header_info =
          (Aws.Util.option_map (Aws.Json.lookup j "FileHeaderInfo")
             FileHeaderInfo.of_json);
        comments =
          (Aws.Util.option_map (Aws.Json.lookup j "Comments") String.of_json);
        quote_escape_character =
          (Aws.Util.option_map (Aws.Json.lookup j "QuoteEscapeCharacter")
             String.of_json);
        record_delimiter =
          (Aws.Util.option_map (Aws.Json.lookup j "RecordDelimiter")
             String.of_json);
        field_delimiter =
          (Aws.Util.option_map (Aws.Json.lookup j "FieldDelimiter")
             String.of_json);
        quote_character =
          (Aws.Util.option_map (Aws.Json.lookup j "QuoteCharacter")
             String.of_json);
        allow_quoted_record_delimiter =
          (Aws.Util.option_map
             (Aws.Json.lookup j "AllowQuotedRecordDelimiter") Boolean.of_json)
      }
  end
module InputSerialization =
  struct
    type t =
      {
      c_s_v: CSVInput.t option ;
      compression_type: CompressionType.t option ;
      j_s_o_n: JSONInput.t option ;
      parquet: ParquetInput.t option }
    let make ?c_s_v  ?compression_type  ?j_s_o_n  ?parquet  () =
      { c_s_v; compression_type; j_s_o_n; parquet }
    let parse xml =
      Some
        {
          c_s_v =
            (Aws.Util.option_bind (Aws.Xml.member "CSV" xml) CSVInput.parse);
          compression_type =
            (Aws.Util.option_bind (Aws.Xml.member "CompressionType" xml)
               CompressionType.parse);
          j_s_o_n =
            (Aws.Util.option_bind (Aws.Xml.member "JSON" xml) JSONInput.parse);
          parquet =
            (Aws.Util.option_bind (Aws.Xml.member "Parquet" xml)
               ParquetInput.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.parquet
              (fun f -> Aws.Query.Pair ("Parquet", (ParquetInput.to_query f)));
           Aws.Util.option_map v.j_s_o_n
             (fun f -> Aws.Query.Pair ("JSON", (JSONInput.to_query f)));
           Aws.Util.option_map v.compression_type
             (fun f ->
                Aws.Query.Pair
                  ("CompressionType", (CompressionType.to_query f)));
           Aws.Util.option_map v.c_s_v
             (fun f -> Aws.Query.Pair ("CSV", (CSVInput.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.parquet
              (fun f -> ("Parquet", (ParquetInput.to_json f)));
           Aws.Util.option_map v.j_s_o_n
             (fun f -> ("JSON", (JSONInput.to_json f)));
           Aws.Util.option_map v.compression_type
             (fun f -> ("CompressionType", (CompressionType.to_json f)));
           Aws.Util.option_map v.c_s_v
             (fun f -> ("CSV", (CSVInput.to_json f)))])
    let of_json j =
      {
        c_s_v =
          (Aws.Util.option_map (Aws.Json.lookup j "CSV") CSVInput.of_json);
        compression_type =
          (Aws.Util.option_map (Aws.Json.lookup j "CompressionType")
             CompressionType.of_json);
        j_s_o_n =
          (Aws.Util.option_map (Aws.Json.lookup j "JSON") JSONInput.of_json);
        parquet =
          (Aws.Util.option_map (Aws.Json.lookup j "Parquet")
             ParquetInput.of_json)
      }
  end
module ExpressionType =
  struct
    type t =
      | SQL 
    let str_to_t = [("SQL", SQL)]
    let t_to_str = [(SQL, "SQL")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module SelectObjectContentRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      expression: String.t ;
      expression_type: ExpressionType.t ;
      request_progress: RequestProgress.t option ;
      input_serialization: InputSerialization.t ;
      output_serialization: OutputSerialization.t ;
      scan_range: ScanRange.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~key  ?s_s_e_customer_algorithm  ?s_s_e_customer_key 
      ?s_s_e_customer_key_m_d5  ~expression  ~expression_type 
      ?request_progress  ~input_serialization  ~output_serialization 
      ?scan_range  ?expected_bucket_owner  () =
      {
        bucket;
        key;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        expression;
        expression_type;
        request_progress;
        input_serialization;
        output_serialization;
        scan_range;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-customer-key"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          expression =
            (Aws.Xml.required "Expression"
               (Aws.Util.option_bind (Aws.Xml.member "Expression" xml)
                  String.parse));
          expression_type =
            (Aws.Xml.required "ExpressionType"
               (Aws.Util.option_bind (Aws.Xml.member "ExpressionType" xml)
                  ExpressionType.parse));
          request_progress =
            (Aws.Util.option_bind (Aws.Xml.member "RequestProgress" xml)
               RequestProgress.parse);
          input_serialization =
            (Aws.Xml.required "InputSerialization"
               (Aws.Util.option_bind
                  (Aws.Xml.member "InputSerialization" xml)
                  InputSerialization.parse));
          output_serialization =
            (Aws.Xml.required "OutputSerialization"
               (Aws.Util.option_bind
                  (Aws.Xml.member "OutputSerialization" xml)
                  OutputSerialization.parse));
          scan_range =
            (Aws.Util.option_bind (Aws.Xml.member "ScanRange" xml)
               ScanRange.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.scan_range
             (fun f -> Aws.Query.Pair ("ScanRange", (ScanRange.to_query f)));
           Some
             (Aws.Query.Pair
                ("OutputSerialization",
                  (OutputSerialization.to_query v.output_serialization)));
           Some
             (Aws.Query.Pair
                ("InputSerialization",
                  (InputSerialization.to_query v.input_serialization)));
           Aws.Util.option_map v.request_progress
             (fun f ->
                Aws.Query.Pair
                  ("RequestProgress", (RequestProgress.to_query f)));
           Some
             (Aws.Query.Pair
                ("ExpressionType",
                  (ExpressionType.to_query v.expression_type)));
           Some
             (Aws.Query.Pair ("Expression", (String.to_query v.expression)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.scan_range
             (fun f -> ("ScanRange", (ScanRange.to_json f)));
           Some
             ("OutputSerialization",
               (OutputSerialization.to_json v.output_serialization));
           Some
             ("InputSerialization",
               (InputSerialization.to_json v.input_serialization));
           Aws.Util.option_map v.request_progress
             (fun f -> ("RequestProgress", (RequestProgress.to_json f)));
           Some
             ("ExpressionType", (ExpressionType.to_json v.expression_type));
           Some ("Expression", (String.to_json v.expression));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                ("x-amz-server-side-encryption-customer-key",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-customer-key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        expression =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Expression")));
        expression_type =
          (ExpressionType.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "ExpressionType")));
        request_progress =
          (Aws.Util.option_map (Aws.Json.lookup j "RequestProgress")
             RequestProgress.of_json);
        input_serialization =
          (InputSerialization.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "InputSerialization")));
        output_serialization =
          (OutputSerialization.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "OutputSerialization")));
        scan_range =
          (Aws.Util.option_map (Aws.Json.lookup j "ScanRange")
             ScanRange.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetObjectRetentionRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      version_id: String.t option ;
      request_payer: RequestPayer.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~key  ?version_id  ?request_payer 
      ?expected_bucket_owner  () =
      { bucket; key; version_id; request_payer; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module ObjectLockLegalHold =
  struct
    type t = {
      status: ObjectLockLegalHoldStatus.t option }
    let make ?status  () = { status }
    let parse xml =
      Some
        {
          status =
            (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
               ObjectLockLegalHoldStatus.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.status
              (fun f ->
                 Aws.Query.Pair
                   ("Status", (ObjectLockLegalHoldStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.status
              (fun f -> ("Status", (ObjectLockLegalHoldStatus.to_json f)))])
    let of_json j =
      {
        status =
          (Aws.Util.option_map (Aws.Json.lookup j "Status")
             ObjectLockLegalHoldStatus.of_json)
      }
  end
module GetObjectLegalHoldOutput =
  struct
    type t = {
      legal_hold: ObjectLockLegalHold.t option }
    let make ?legal_hold  () = { legal_hold }
    let parse xml =
      Some
        {
          legal_hold =
            (Aws.Util.option_bind (Aws.Xml.member "LegalHold" xml)
               ObjectLockLegalHold.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.legal_hold
              (fun f ->
                 Aws.Query.Pair
                   ("LegalHold", (ObjectLockLegalHold.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.legal_hold
              (fun f -> ("LegalHold", (ObjectLockLegalHold.to_json f)))])
    let of_json j =
      {
        legal_hold =
          (Aws.Util.option_map (Aws.Json.lookup j "LegalHold")
             ObjectLockLegalHold.of_json)
      }
  end
module ExposeHeaders =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module AllowedHeaders =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map String.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Aws.Json.to_list String.of_json j
  end
module CORSRule =
  struct
    type t =
      {
      i_d: String.t option ;
      allowed_headers: AllowedHeaders.t ;
      allowed_methods: AllowedMethods.t ;
      allowed_origins: AllowedOrigins.t ;
      expose_headers: ExposeHeaders.t ;
      max_age_seconds: Integer.t option }
    let make ?i_d  ?(allowed_headers= [])  ~allowed_methods  ~allowed_origins
       ?(expose_headers= [])  ?max_age_seconds  () =
      {
        i_d;
        allowed_headers;
        allowed_methods;
        allowed_origins;
        expose_headers;
        max_age_seconds
      }
    let parse xml =
      Some
        {
          i_d = (Aws.Util.option_bind (Aws.Xml.member "ID" xml) String.parse);
          allowed_headers =
            (Aws.Util.of_option [] (AllowedHeaders.parse xml));
          allowed_methods =
            (Aws.Xml.required "AllowedMethod" (AllowedMethods.parse xml));
          allowed_origins =
            (Aws.Xml.required "AllowedOrigin" (AllowedOrigins.parse xml));
          expose_headers = (Aws.Util.of_option [] (ExposeHeaders.parse xml));
          max_age_seconds =
            (Aws.Util.option_bind (Aws.Xml.member "MaxAgeSeconds" xml)
               Integer.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.max_age_seconds
              (fun f ->
                 Aws.Query.Pair ("MaxAgeSeconds", (Integer.to_query f)));
           Some
             (Aws.Query.Pair
                ("ExposeHeader", (ExposeHeaders.to_query v.expose_headers)));
           Some
             (Aws.Query.Pair
                ("AllowedOrigin",
                  (AllowedOrigins.to_query v.allowed_origins)));
           Some
             (Aws.Query.Pair
                ("AllowedMethod",
                  (AllowedMethods.to_query v.allowed_methods)));
           Some
             (Aws.Query.Pair
                ("AllowedHeader",
                  (AllowedHeaders.to_query v.allowed_headers)));
           Aws.Util.option_map v.i_d
             (fun f -> Aws.Query.Pair ("ID", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.max_age_seconds
              (fun f -> ("MaxAgeSeconds", (Integer.to_json f)));
           Some ("ExposeHeader", (ExposeHeaders.to_json v.expose_headers));
           Some ("AllowedOrigin", (AllowedOrigins.to_json v.allowed_origins));
           Some ("AllowedMethod", (AllowedMethods.to_json v.allowed_methods));
           Some ("AllowedHeader", (AllowedHeaders.to_json v.allowed_headers));
           Aws.Util.option_map v.i_d (fun f -> ("ID", (String.to_json f)))])
    let of_json j =
      {
        i_d = (Aws.Util.option_map (Aws.Json.lookup j "ID") String.of_json);
        allowed_headers =
          (AllowedHeaders.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AllowedHeader")));
        allowed_methods =
          (AllowedMethods.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AllowedMethod")));
        allowed_origins =
          (AllowedOrigins.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AllowedOrigin")));
        expose_headers =
          (ExposeHeaders.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "ExposeHeader")));
        max_age_seconds =
          (Aws.Util.option_map (Aws.Json.lookup j "MaxAgeSeconds")
             Integer.of_json)
      }
  end
module CORSRules =
  struct
    type t = CORSRule.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map CORSRule.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list CORSRule.to_query v
    let to_json v = `List (List.map CORSRule.to_json v)
    let of_json j = Aws.Json.to_list CORSRule.of_json j
  end
module Payer =
  struct
    type t =
      | Requester 
      | BucketOwner 
    let str_to_t = [("BucketOwner", BucketOwner); ("Requester", Requester)]
    let t_to_str = [(BucketOwner, "BucketOwner"); (Requester, "Requester")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module RequestPaymentConfiguration =
  struct
    type t = {
      payer: Payer.t }
    let make ~payer  () = { payer }
    let parse xml =
      Some
        {
          payer =
            (Aws.Xml.required "Payer"
               (Aws.Util.option_bind (Aws.Xml.member "Payer" xml) Payer.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Payer", (Payer.to_query v.payer)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt [Some ("Payer", (Payer.to_json v.payer))])
    let of_json j =
      {
        payer =
          (Payer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Payer")))
      }
  end
module Stats =
  struct
    type t =
      {
      bytes_scanned: Long.t option ;
      bytes_processed: Long.t option ;
      bytes_returned: Long.t option }
    let make ?bytes_scanned  ?bytes_processed  ?bytes_returned  () =
      { bytes_scanned; bytes_processed; bytes_returned }
    let parse xml =
      Some
        {
          bytes_scanned =
            (Aws.Util.option_bind (Aws.Xml.member "BytesScanned" xml)
               Long.parse);
          bytes_processed =
            (Aws.Util.option_bind (Aws.Xml.member "BytesProcessed" xml)
               Long.parse);
          bytes_returned =
            (Aws.Util.option_bind (Aws.Xml.member "BytesReturned" xml)
               Long.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.bytes_returned
              (fun f -> Aws.Query.Pair ("BytesReturned", (Long.to_query f)));
           Aws.Util.option_map v.bytes_processed
             (fun f -> Aws.Query.Pair ("BytesProcessed", (Long.to_query f)));
           Aws.Util.option_map v.bytes_scanned
             (fun f -> Aws.Query.Pair ("BytesScanned", (Long.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.bytes_returned
              (fun f -> ("BytesReturned", (Long.to_json f)));
           Aws.Util.option_map v.bytes_processed
             (fun f -> ("BytesProcessed", (Long.to_json f)));
           Aws.Util.option_map v.bytes_scanned
             (fun f -> ("BytesScanned", (Long.to_json f)))])
    let of_json j =
      {
        bytes_scanned =
          (Aws.Util.option_map (Aws.Json.lookup j "BytesScanned")
             Long.of_json);
        bytes_processed =
          (Aws.Util.option_map (Aws.Json.lookup j "BytesProcessed")
             Long.of_json);
        bytes_returned =
          (Aws.Util.option_map (Aws.Json.lookup j "BytesReturned")
             Long.of_json)
      }
  end
module StatsEvent =
  struct
    type t = {
      details: Stats.t option }
    let make ?details  () = { details }
    let parse xml =
      Some
        {
          details =
            (Aws.Util.option_bind (Aws.Xml.member "Details" xml) Stats.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.details
              (fun f -> Aws.Query.Pair ("Details", (Stats.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.details
              (fun f -> ("Details", (Stats.to_json f)))])
    let of_json j =
      {
        details =
          (Aws.Util.option_map (Aws.Json.lookup j "Details") Stats.of_json)
      }
  end
module RecordsEvent =
  struct
    type t = {
      payload: Blob.t option }
    let make ?payload  () = { payload }
    let parse xml =
      Some
        {
          payload =
            (Aws.Util.option_bind (Aws.Xml.member "Payload" xml) Blob.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.payload
              (fun f -> Aws.Query.Pair ("Payload", (Blob.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.payload
              (fun f -> ("Payload", (Blob.to_json f)))])
    let of_json j =
      {
        payload =
          (Aws.Util.option_map (Aws.Json.lookup j "Payload") Blob.of_json)
      }
  end
module Progress =
  struct
    type t =
      {
      bytes_scanned: Long.t option ;
      bytes_processed: Long.t option ;
      bytes_returned: Long.t option }
    let make ?bytes_scanned  ?bytes_processed  ?bytes_returned  () =
      { bytes_scanned; bytes_processed; bytes_returned }
    let parse xml =
      Some
        {
          bytes_scanned =
            (Aws.Util.option_bind (Aws.Xml.member "BytesScanned" xml)
               Long.parse);
          bytes_processed =
            (Aws.Util.option_bind (Aws.Xml.member "BytesProcessed" xml)
               Long.parse);
          bytes_returned =
            (Aws.Util.option_bind (Aws.Xml.member "BytesReturned" xml)
               Long.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.bytes_returned
              (fun f -> Aws.Query.Pair ("BytesReturned", (Long.to_query f)));
           Aws.Util.option_map v.bytes_processed
             (fun f -> Aws.Query.Pair ("BytesProcessed", (Long.to_query f)));
           Aws.Util.option_map v.bytes_scanned
             (fun f -> Aws.Query.Pair ("BytesScanned", (Long.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.bytes_returned
              (fun f -> ("BytesReturned", (Long.to_json f)));
           Aws.Util.option_map v.bytes_processed
             (fun f -> ("BytesProcessed", (Long.to_json f)));
           Aws.Util.option_map v.bytes_scanned
             (fun f -> ("BytesScanned", (Long.to_json f)))])
    let of_json j =
      {
        bytes_scanned =
          (Aws.Util.option_map (Aws.Json.lookup j "BytesScanned")
             Long.of_json);
        bytes_processed =
          (Aws.Util.option_map (Aws.Json.lookup j "BytesProcessed")
             Long.of_json);
        bytes_returned =
          (Aws.Util.option_map (Aws.Json.lookup j "BytesReturned")
             Long.of_json)
      }
  end
module ProgressEvent =
  struct
    type t = {
      details: Progress.t option }
    let make ?details  () = { details }
    let parse xml =
      Some
        {
          details =
            (Aws.Util.option_bind (Aws.Xml.member "Details" xml)
               Progress.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.details
              (fun f -> Aws.Query.Pair ("Details", (Progress.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.details
              (fun f -> ("Details", (Progress.to_json f)))])
    let of_json j =
      {
        details =
          (Aws.Util.option_map (Aws.Json.lookup j "Details") Progress.of_json)
      }
  end
module EndEvent =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])
    let to_json v = `Assoc (Aws.Util.list_filter_opt [])
    let of_json j = ()
  end
module ContinuationEvent =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])
    let to_json v = `Assoc (Aws.Util.list_filter_opt [])
    let of_json j = ()
  end
module SelectObjectContentEventStream =
  struct
    type t =
      {
      records: RecordsEvent.t option ;
      stats: StatsEvent.t option ;
      progress: ProgressEvent.t option ;
      cont: ContinuationEvent.t option ;
      end_: EndEvent.t option }
    let make ?records  ?stats  ?progress  ?cont  ?end_  () =
      { records; stats; progress; cont; end_ }
    let parse xml =
      Some
        {
          records =
            (Aws.Util.option_bind (Aws.Xml.member "Records" xml)
               RecordsEvent.parse);
          stats =
            (Aws.Util.option_bind (Aws.Xml.member "Stats" xml)
               StatsEvent.parse);
          progress =
            (Aws.Util.option_bind (Aws.Xml.member "Progress" xml)
               ProgressEvent.parse);
          cont =
            (Aws.Util.option_bind (Aws.Xml.member "Cont" xml)
               ContinuationEvent.parse);
          end_ =
            (Aws.Util.option_bind (Aws.Xml.member "End" xml) EndEvent.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.end_
              (fun f -> Aws.Query.Pair ("End", (EndEvent.to_query f)));
           Aws.Util.option_map v.cont
             (fun f ->
                Aws.Query.Pair ("Cont", (ContinuationEvent.to_query f)));
           Aws.Util.option_map v.progress
             (fun f ->
                Aws.Query.Pair ("Progress", (ProgressEvent.to_query f)));
           Aws.Util.option_map v.stats
             (fun f -> Aws.Query.Pair ("Stats", (StatsEvent.to_query f)));
           Aws.Util.option_map v.records
             (fun f -> Aws.Query.Pair ("Records", (RecordsEvent.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.end_
              (fun f -> ("End", (EndEvent.to_json f)));
           Aws.Util.option_map v.cont
             (fun f -> ("Cont", (ContinuationEvent.to_json f)));
           Aws.Util.option_map v.progress
             (fun f -> ("Progress", (ProgressEvent.to_json f)));
           Aws.Util.option_map v.stats
             (fun f -> ("Stats", (StatsEvent.to_json f)));
           Aws.Util.option_map v.records
             (fun f -> ("Records", (RecordsEvent.to_json f)))])
    let of_json j =
      {
        records =
          (Aws.Util.option_map (Aws.Json.lookup j "Records")
             RecordsEvent.of_json);
        stats =
          (Aws.Util.option_map (Aws.Json.lookup j "Stats") StatsEvent.of_json);
        progress =
          (Aws.Util.option_map (Aws.Json.lookup j "Progress")
             ProgressEvent.of_json);
        cont =
          (Aws.Util.option_map (Aws.Json.lookup j "Cont")
             ContinuationEvent.of_json);
        end_ =
          (Aws.Util.option_map (Aws.Json.lookup j "End") EndEvent.of_json)
      }
  end
module SelectObjectContentOutput =
  struct
    type t = {
      payload: SelectObjectContentEventStream.t option }
    let make ?payload  () = { payload }
    let parse xml =
      Some
        {
          payload =
            (Aws.Util.option_bind (Aws.Xml.member "Payload" xml)
               SelectObjectContentEventStream.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.payload
              (fun f ->
                 Aws.Query.Pair
                   ("Payload", (SelectObjectContentEventStream.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.payload
              (fun f ->
                 ("Payload", (SelectObjectContentEventStream.to_json f)))])
    let of_json j =
      {
        payload =
          (Aws.Util.option_map (Aws.Json.lookup j "Payload")
             SelectObjectContentEventStream.of_json)
      }
  end
module EncryptionConfiguration =
  struct
    type t = {
      replica_kms_key_i_d: String.t option }
    let make ?replica_kms_key_i_d  () = { replica_kms_key_i_d }
    let parse xml =
      Some
        {
          replica_kms_key_i_d =
            (Aws.Util.option_bind (Aws.Xml.member "ReplicaKmsKeyID" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.replica_kms_key_i_d
              (fun f ->
                 Aws.Query.Pair ("ReplicaKmsKeyID", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.replica_kms_key_i_d
              (fun f -> ("ReplicaKmsKeyID", (String.to_json f)))])
    let of_json j =
      {
        replica_kms_key_i_d =
          (Aws.Util.option_map (Aws.Json.lookup j "ReplicaKmsKeyID")
             String.of_json)
      }
  end
module GetBucketTaggingOutput =
  struct
    type t = {
      tag_set: TagSet.t }
    let make ~tag_set  () = { tag_set }
    let parse xml =
      Some
        {
          tag_set =
            (Aws.Xml.required "TagSet"
               (Aws.Util.option_bind (Aws.Xml.member "TagSet" xml)
                  TagSet.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair ("TagSet.member", (TagSet.to_query v.tag_set)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("TagSet", (TagSet.to_json v.tag_set))])
    let of_json j =
      {
        tag_set =
          (TagSet.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSet")))
      }
  end
module PutObjectOutput =
  struct
    type t =
      {
      expiration: String.t option ;
      e_tag: String.t option ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      version_id: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      s_s_e_k_m_s_encryption_context: String.t option ;
      bucket_key_enabled: Boolean.t option ;
      request_charged: RequestCharged.t option }
    let make ?expiration  ?e_tag  ?checksum_c_r_c32  ?checksum_c_r_c32_c 
      ?checksum_s_h_a1  ?checksum_s_h_a256  ?server_side_encryption 
      ?version_id  ?s_s_e_customer_algorithm  ?s_s_e_customer_key_m_d5 
      ?s_s_e_k_m_s_key_id  ?s_s_e_k_m_s_encryption_context 
      ?bucket_key_enabled  ?request_charged  () =
      {
        expiration;
        e_tag;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256;
        server_side_encryption;
        version_id;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        s_s_e_k_m_s_encryption_context;
        bucket_key_enabled;
        request_charged
      }
    let parse xml =
      Some
        {
          expiration =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-expiration" xml)
               String.parse);
          e_tag =
            (Aws.Util.option_bind (Aws.Xml.member "ETag" xml) String.parse);
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-crc32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-crc32c" xml) String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-sha1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-sha256" xml) String.parse);
          server_side_encryption =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-version-id" xml)
               String.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          s_s_e_k_m_s_key_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-aws-kms-key-id"
                  xml) String.parse);
          s_s_e_k_m_s_encryption_context =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-context" xml)
               String.parse);
          bucket_key_enabled =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-bucket-key-enabled" xml)
               Boolean.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-bucket-key-enabled",
                    (Boolean.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_encryption_context
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-context",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f ->
                Aws.Query.Pair ("x-amz-version-id", (String.to_query f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32c", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32", (String.to_query f)));
           Aws.Util.option_map v.e_tag
             (fun f -> Aws.Query.Pair ("ETag", (String.to_query f)));
           Aws.Util.option_map v.expiration
             (fun f ->
                Aws.Query.Pair ("x-amz-expiration", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                ("x-amz-server-side-encryption-bucket-key-enabled",
                  (Boolean.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_encryption_context
             (fun f ->
                ("x-amz-server-side-encryption-context", (String.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                ("x-amz-server-side-encryption-aws-kms-key-id",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("x-amz-version-id", (String.to_json f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                ("x-amz-server-side-encryption",
                  (ServerSideEncryption.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f -> ("x-amz-checksum-sha256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("x-amz-checksum-sha1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("x-amz-checksum-crc32c", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("x-amz-checksum-crc32", (String.to_json f)));
           Aws.Util.option_map v.e_tag
             (fun f -> ("ETag", (String.to_json f)));
           Aws.Util.option_map v.expiration
             (fun f -> ("x-amz-expiration", (String.to_json f)))])
    let of_json j =
      {
        expiration =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-expiration")
             String.of_json);
        e_tag =
          (Aws.Util.option_map (Aws.Json.lookup j "ETag") String.of_json);
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32c")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha256")
             String.of_json);
        server_side_encryption =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption")
             ServerSideEncryption.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-version-id")
             String.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-aws-kms-key-id")
             String.of_json);
        s_s_e_k_m_s_encryption_context =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-context")
             String.of_json);
        bucket_key_enabled =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-bucket-key-enabled")
             Boolean.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module TopicConfigurationDeprecated =
  struct
    type t =
      {
      id: String.t option ;
      events: EventList.t ;
      event: Event.t option ;
      topic: String.t option }
    let make ?id  ?(events= [])  ?event  ?topic  () =
      { id; events; event; topic }
    let parse xml =
      Some
        {
          id = (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse);
          events = (Aws.Util.of_option [] (EventList.parse xml));
          event =
            (Aws.Util.option_bind (Aws.Xml.member "Event" xml) Event.parse);
          topic =
            (Aws.Util.option_bind (Aws.Xml.member "Topic" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.topic
              (fun f -> Aws.Query.Pair ("Topic", (String.to_query f)));
           Aws.Util.option_map v.event
             (fun f -> Aws.Query.Pair ("Event", (Event.to_query f)));
           Some (Aws.Query.Pair ("Event", (EventList.to_query v.events)));
           Aws.Util.option_map v.id
             (fun f -> Aws.Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.topic
              (fun f -> ("Topic", (String.to_json f)));
           Aws.Util.option_map v.event
             (fun f -> ("Event", (Event.to_json f)));
           Some ("Event", (EventList.to_json v.events));
           Aws.Util.option_map v.id (fun f -> ("Id", (String.to_json f)))])
    let of_json j =
      {
        id = (Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json);
        events =
          (EventList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Event")));
        event =
          (Aws.Util.option_map (Aws.Json.lookup j "Event") Event.of_json);
        topic =
          (Aws.Util.option_map (Aws.Json.lookup j "Topic") String.of_json)
      }
  end
module QueueConfigurationDeprecated =
  struct
    type t =
      {
      id: String.t option ;
      event: Event.t option ;
      events: EventList.t ;
      queue: String.t option }
    let make ?id  ?event  ?(events= [])  ?queue  () =
      { id; event; events; queue }
    let parse xml =
      Some
        {
          id = (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse);
          event =
            (Aws.Util.option_bind (Aws.Xml.member "Event" xml) Event.parse);
          events = (Aws.Util.of_option [] (EventList.parse xml));
          queue =
            (Aws.Util.option_bind (Aws.Xml.member "Queue" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.queue
              (fun f -> Aws.Query.Pair ("Queue", (String.to_query f)));
           Some (Aws.Query.Pair ("Event", (EventList.to_query v.events)));
           Aws.Util.option_map v.event
             (fun f -> Aws.Query.Pair ("Event", (Event.to_query f)));
           Aws.Util.option_map v.id
             (fun f -> Aws.Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.queue
              (fun f -> ("Queue", (String.to_json f)));
           Some ("Event", (EventList.to_json v.events));
           Aws.Util.option_map v.event
             (fun f -> ("Event", (Event.to_json f)));
           Aws.Util.option_map v.id (fun f -> ("Id", (String.to_json f)))])
    let of_json j =
      {
        id = (Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json);
        event =
          (Aws.Util.option_map (Aws.Json.lookup j "Event") Event.of_json);
        events =
          (EventList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Event")));
        queue =
          (Aws.Util.option_map (Aws.Json.lookup j "Queue") String.of_json)
      }
  end
module NotificationConfigurationDeprecated =
  struct
    type t =
      {
      topic_configuration: TopicConfigurationDeprecated.t option ;
      queue_configuration: QueueConfigurationDeprecated.t option ;
      cloud_function_configuration: CloudFunctionConfiguration.t option }
    let make ?topic_configuration  ?queue_configuration 
      ?cloud_function_configuration  () =
      {
        topic_configuration;
        queue_configuration;
        cloud_function_configuration
      }
    let parse xml =
      Some
        {
          topic_configuration =
            (Aws.Util.option_bind (Aws.Xml.member "TopicConfiguration" xml)
               TopicConfigurationDeprecated.parse);
          queue_configuration =
            (Aws.Util.option_bind (Aws.Xml.member "QueueConfiguration" xml)
               QueueConfigurationDeprecated.parse);
          cloud_function_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "CloudFunctionConfiguration" xml)
               CloudFunctionConfiguration.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.cloud_function_configuration
              (fun f ->
                 Aws.Query.Pair
                   ("CloudFunctionConfiguration",
                     (CloudFunctionConfiguration.to_query f)));
           Aws.Util.option_map v.queue_configuration
             (fun f ->
                Aws.Query.Pair
                  ("QueueConfiguration",
                    (QueueConfigurationDeprecated.to_query f)));
           Aws.Util.option_map v.topic_configuration
             (fun f ->
                Aws.Query.Pair
                  ("TopicConfiguration",
                    (TopicConfigurationDeprecated.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.cloud_function_configuration
              (fun f ->
                 ("CloudFunctionConfiguration",
                   (CloudFunctionConfiguration.to_json f)));
           Aws.Util.option_map v.queue_configuration
             (fun f ->
                ("QueueConfiguration",
                  (QueueConfigurationDeprecated.to_json f)));
           Aws.Util.option_map v.topic_configuration
             (fun f ->
                ("TopicConfiguration",
                  (TopicConfigurationDeprecated.to_json f)))])
    let of_json j =
      {
        topic_configuration =
          (Aws.Util.option_map (Aws.Json.lookup j "TopicConfiguration")
             TopicConfigurationDeprecated.of_json);
        queue_configuration =
          (Aws.Util.option_map (Aws.Json.lookup j "QueueConfiguration")
             QueueConfigurationDeprecated.of_json);
        cloud_function_configuration =
          (Aws.Util.option_map
             (Aws.Json.lookup j "CloudFunctionConfiguration")
             CloudFunctionConfiguration.of_json)
      }
  end
module PutBucketNotificationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      notification_configuration: NotificationConfigurationDeprecated.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?content_m_d5  ?checksum_algorithm 
      ~notification_configuration  ?expected_bucket_owner  () =
      {
        bucket;
        content_m_d5;
        checksum_algorithm;
        notification_configuration;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          notification_configuration =
            (Aws.Xml.required "NotificationConfiguration"
               (Aws.Util.option_bind
                  (Aws.Xml.member "NotificationConfiguration" xml)
                  NotificationConfigurationDeprecated.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("NotificationConfiguration",
                  (NotificationConfigurationDeprecated.to_query
                     v.notification_configuration)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some
             ("NotificationConfiguration",
               (NotificationConfigurationDeprecated.to_json
                  v.notification_configuration));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        notification_configuration =
          (NotificationConfigurationDeprecated.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "NotificationConfiguration")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetBucketAccelerateConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      expected_bucket_owner: String.t option ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ?expected_bucket_owner  ?request_payer  () =
      { bucket; expected_bucket_owner; request_payer }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_payer
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_payer
              (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json)
      }
  end
module GetBucketAccelerateConfigurationOutput =
  struct
    type t =
      {
      status: BucketAccelerateStatus.t option ;
      request_charged: RequestCharged.t option }
    let make ?status  ?request_charged  () = { status; request_charged }
    let parse xml =
      Some
        {
          status =
            (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
               BucketAccelerateStatus.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.status
             (fun f ->
                Aws.Query.Pair
                  ("Status", (BucketAccelerateStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.status
             (fun f -> ("Status", (BucketAccelerateStatus.to_json f)))])
    let of_json j =
      {
        status =
          (Aws.Util.option_map (Aws.Json.lookup j "Status")
             BucketAccelerateStatus.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module PutBucketPolicyRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      confirm_remove_self_bucket_access: Boolean.t option ;
      policy: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?content_m_d5  ?checksum_algorithm 
      ?confirm_remove_self_bucket_access  ~policy  ?expected_bucket_owner  ()
      =
      {
        bucket;
        content_m_d5;
        checksum_algorithm;
        confirm_remove_self_bucket_access;
        policy;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          confirm_remove_self_bucket_access =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-confirm-remove-self-bucket-access" xml)
               Boolean.parse);
          policy =
            (Aws.Xml.required "Policy"
               (Aws.Util.option_bind (Aws.Xml.member "Policy" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Policy", (String.to_query v.policy)));
           Aws.Util.option_map v.confirm_remove_self_bucket_access
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-confirm-remove-self-bucket-access",
                    (Boolean.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Policy", (String.to_json v.policy));
           Aws.Util.option_map v.confirm_remove_self_bucket_access
             (fun f ->
                ("x-amz-confirm-remove-self-bucket-access",
                  (Boolean.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        confirm_remove_self_bucket_access =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-confirm-remove-self-bucket-access")
             Boolean.of_json);
        policy =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Policy")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module CORSConfiguration =
  struct
    type t = {
      c_o_r_s_rules: CORSRules.t }
    let make ~c_o_r_s_rules  () = { c_o_r_s_rules }
    let parse xml =
      Some
        { c_o_r_s_rules = (Aws.Xml.required "CORSRule" (CORSRules.parse xml))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("CORSRule", (CORSRules.to_query v.c_o_r_s_rules)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("CORSRule", (CORSRules.to_json v.c_o_r_s_rules))])
    let of_json j =
      {
        c_o_r_s_rules =
          (CORSRules.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "CORSRule")))
      }
  end
module CreateMultipartUploadRequest =
  struct
    type t =
      {
      a_c_l: ObjectCannedACL.t option ;
      bucket: String.t ;
      cache_control: String.t option ;
      content_disposition: String.t option ;
      content_encoding: String.t option ;
      content_language: String.t option ;
      content_type: String.t option ;
      expires: DateTime.t option ;
      grant_full_control: String.t option ;
      grant_read: String.t option ;
      grant_read_a_c_p: String.t option ;
      grant_write_a_c_p: String.t option ;
      key: String.t ;
      metadata: Metadata.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      storage_class: StorageClass.t option ;
      website_redirect_location: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      s_s_e_k_m_s_encryption_context: String.t option ;
      bucket_key_enabled: Boolean.t option ;
      request_payer: RequestPayer.t option ;
      tagging: String.t option ;
      object_lock_mode: ObjectLockMode.t option ;
      object_lock_retain_until_date: DateTime.t option ;
      object_lock_legal_hold_status: ObjectLockLegalHoldStatus.t option ;
      expected_bucket_owner: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option }
    let make ?a_c_l  ~bucket  ?cache_control  ?content_disposition 
      ?content_encoding  ?content_language  ?content_type  ?expires 
      ?grant_full_control  ?grant_read  ?grant_read_a_c_p  ?grant_write_a_c_p
       ~key  ?metadata  ?server_side_encryption  ?storage_class 
      ?website_redirect_location  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key  ?s_s_e_customer_key_m_d5  ?s_s_e_k_m_s_key_id 
      ?s_s_e_k_m_s_encryption_context  ?bucket_key_enabled  ?request_payer 
      ?tagging  ?object_lock_mode  ?object_lock_retain_until_date 
      ?object_lock_legal_hold_status  ?expected_bucket_owner 
      ?checksum_algorithm  () =
      {
        a_c_l;
        bucket;
        cache_control;
        content_disposition;
        content_encoding;
        content_language;
        content_type;
        expires;
        grant_full_control;
        grant_read;
        grant_read_a_c_p;
        grant_write_a_c_p;
        key;
        metadata;
        server_side_encryption;
        storage_class;
        website_redirect_location;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        s_s_e_k_m_s_encryption_context;
        bucket_key_enabled;
        request_payer;
        tagging;
        object_lock_mode;
        object_lock_retain_until_date;
        object_lock_legal_hold_status;
        expected_bucket_owner;
        checksum_algorithm
      }
    let parse xml =
      Some
        {
          a_c_l =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-acl" xml)
               ObjectCannedACL.parse);
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          cache_control =
            (Aws.Util.option_bind (Aws.Xml.member "Cache-Control" xml)
               String.parse);
          content_disposition =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Disposition" xml)
               String.parse);
          content_encoding =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Encoding" xml)
               String.parse);
          content_language =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Language" xml)
               String.parse);
          content_type =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Type" xml)
               String.parse);
          expires =
            (Aws.Util.option_bind (Aws.Xml.member "Expires" xml)
               DateTime.parse);
          grant_full_control =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-grant-full-control" xml) String.parse);
          grant_read =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-read" xml)
               String.parse);
          grant_read_a_c_p =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-read-acp" xml)
               String.parse);
          grant_write_a_c_p =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-grant-write-acp" xml) String.parse);
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          metadata =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-meta-" xml)
               Metadata.parse);
          server_side_encryption =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-storage-class" xml)
               StorageClass.parse);
          website_redirect_location =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-website-redirect-location" xml)
               String.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-customer-key"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          s_s_e_k_m_s_key_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-aws-kms-key-id"
                  xml) String.parse);
          s_s_e_k_m_s_encryption_context =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-context" xml)
               String.parse);
          bucket_key_enabled =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-bucket-key-enabled" xml)
               Boolean.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          tagging =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-tagging" xml)
               String.parse);
          object_lock_mode =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-mode" xml)
               ObjectLockMode.parse);
          object_lock_retain_until_date =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-retain-until-date" xml)
               DateTime.parse);
          object_lock_legal_hold_status =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-legal-hold" xml)
               ObjectLockLegalHoldStatus.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-algorithm" xml)
               ChecksumAlgorithm.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_algorithm
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-checksum-algorithm",
                     (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.object_lock_legal_hold_status
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-object-lock-legal-hold",
                    (ObjectLockLegalHoldStatus.to_query f)));
           Aws.Util.option_map v.object_lock_retain_until_date
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-object-lock-retain-until-date",
                    (DateTime.to_query f)));
           Aws.Util.option_map v.object_lock_mode
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-object-lock-mode", (ObjectLockMode.to_query f)));
           Aws.Util.option_map v.tagging
             (fun f -> Aws.Query.Pair ("x-amz-tagging", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-bucket-key-enabled",
                    (Boolean.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_encryption_context
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-context",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.website_redirect_location
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-website-redirect-location", (String.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-storage-class", (StorageClass.to_query f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Aws.Util.option_map v.metadata
             (fun f -> Aws.Query.Pair ("x-amz-meta-", (Metadata.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Aws.Util.option_map v.grant_write_a_c_p
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-write-acp", (String.to_query f)));
           Aws.Util.option_map v.grant_read_a_c_p
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-read-acp", (String.to_query f)));
           Aws.Util.option_map v.grant_read
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-read", (String.to_query f)));
           Aws.Util.option_map v.grant_full_control
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-grant-full-control", (String.to_query f)));
           Aws.Util.option_map v.expires
             (fun f -> Aws.Query.Pair ("Expires", (DateTime.to_query f)));
           Aws.Util.option_map v.content_type
             (fun f -> Aws.Query.Pair ("Content-Type", (String.to_query f)));
           Aws.Util.option_map v.content_language
             (fun f ->
                Aws.Query.Pair ("Content-Language", (String.to_query f)));
           Aws.Util.option_map v.content_encoding
             (fun f ->
                Aws.Query.Pair ("Content-Encoding", (String.to_query f)));
           Aws.Util.option_map v.content_disposition
             (fun f ->
                Aws.Query.Pair ("Content-Disposition", (String.to_query f)));
           Aws.Util.option_map v.cache_control
             (fun f -> Aws.Query.Pair ("Cache-Control", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)));
           Aws.Util.option_map v.a_c_l
             (fun f ->
                Aws.Query.Pair ("x-amz-acl", (ObjectCannedACL.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_algorithm
              (fun f ->
                 ("x-amz-checksum-algorithm", (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.object_lock_legal_hold_status
             (fun f ->
                ("x-amz-object-lock-legal-hold",
                  (ObjectLockLegalHoldStatus.to_json f)));
           Aws.Util.option_map v.object_lock_retain_until_date
             (fun f ->
                ("x-amz-object-lock-retain-until-date", (DateTime.to_json f)));
           Aws.Util.option_map v.object_lock_mode
             (fun f -> ("x-amz-object-lock-mode", (ObjectLockMode.to_json f)));
           Aws.Util.option_map v.tagging
             (fun f -> ("x-amz-tagging", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                ("x-amz-server-side-encryption-bucket-key-enabled",
                  (Boolean.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_encryption_context
             (fun f ->
                ("x-amz-server-side-encryption-context", (String.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                ("x-amz-server-side-encryption-aws-kms-key-id",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                ("x-amz-server-side-encryption-customer-key",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.website_redirect_location
             (fun f ->
                ("x-amz-website-redirect-location", (String.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f -> ("x-amz-storage-class", (StorageClass.to_json f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                ("x-amz-server-side-encryption",
                  (ServerSideEncryption.to_json f)));
           Aws.Util.option_map v.metadata
             (fun f -> ("x-amz-meta-", (Metadata.to_json f)));
           Some ("Key", (String.to_json v.key));
           Aws.Util.option_map v.grant_write_a_c_p
             (fun f -> ("x-amz-grant-write-acp", (String.to_json f)));
           Aws.Util.option_map v.grant_read_a_c_p
             (fun f -> ("x-amz-grant-read-acp", (String.to_json f)));
           Aws.Util.option_map v.grant_read
             (fun f -> ("x-amz-grant-read", (String.to_json f)));
           Aws.Util.option_map v.grant_full_control
             (fun f -> ("x-amz-grant-full-control", (String.to_json f)));
           Aws.Util.option_map v.expires
             (fun f -> ("Expires", (DateTime.to_json f)));
           Aws.Util.option_map v.content_type
             (fun f -> ("Content-Type", (String.to_json f)));
           Aws.Util.option_map v.content_language
             (fun f -> ("Content-Language", (String.to_json f)));
           Aws.Util.option_map v.content_encoding
             (fun f -> ("Content-Encoding", (String.to_json f)));
           Aws.Util.option_map v.content_disposition
             (fun f -> ("Content-Disposition", (String.to_json f)));
           Aws.Util.option_map v.cache_control
             (fun f -> ("Cache-Control", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket));
           Aws.Util.option_map v.a_c_l
             (fun f -> ("x-amz-acl", (ObjectCannedACL.to_json f)))])
    let of_json j =
      {
        a_c_l =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-acl")
             ObjectCannedACL.of_json);
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        cache_control =
          (Aws.Util.option_map (Aws.Json.lookup j "Cache-Control")
             String.of_json);
        content_disposition =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Disposition")
             String.of_json);
        content_encoding =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Encoding")
             String.of_json);
        content_language =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Language")
             String.of_json);
        content_type =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Type")
             String.of_json);
        expires =
          (Aws.Util.option_map (Aws.Json.lookup j "Expires") DateTime.of_json);
        grant_full_control =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-full-control")
             String.of_json);
        grant_read =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-read")
             String.of_json);
        grant_read_a_c_p =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-read-acp")
             String.of_json);
        grant_write_a_c_p =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-write-acp")
             String.of_json);
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        metadata =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-meta-")
             Metadata.of_json);
        server_side_encryption =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption")
             ServerSideEncryption.of_json);
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-storage-class")
             StorageClass.of_json);
        website_redirect_location =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-website-redirect-location")
             String.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-customer-key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-aws-kms-key-id")
             String.of_json);
        s_s_e_k_m_s_encryption_context =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-context")
             String.of_json);
        bucket_key_enabled =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-bucket-key-enabled")
             Boolean.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        tagging =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-tagging")
             String.of_json);
        object_lock_mode =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-object-lock-mode")
             ObjectLockMode.of_json);
        object_lock_retain_until_date =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-object-lock-retain-until-date")
             DateTime.of_json);
        object_lock_legal_hold_status =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-object-lock-legal-hold")
             ObjectLockLegalHoldStatus.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-algorithm")
             ChecksumAlgorithm.of_json)
      }
  end
module BucketAlreadyExists =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])
    let to_json v = `Assoc (Aws.Util.list_filter_opt [])
    let of_json j = ()
  end
module Bucket =
  struct
    type t = {
      name: String.t option ;
      creation_date: DateTime.t option }
    let make ?name  ?creation_date  () = { name; creation_date }
    let parse xml =
      Some
        {
          name =
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse);
          creation_date =
            (Aws.Util.option_bind (Aws.Xml.member "CreationDate" xml)
               DateTime.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.creation_date
              (fun f ->
                 Aws.Query.Pair ("CreationDate", (DateTime.to_query f)));
           Aws.Util.option_map v.name
             (fun f -> Aws.Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.creation_date
              (fun f -> ("CreationDate", (DateTime.to_json f)));
           Aws.Util.option_map v.name (fun f -> ("Name", (String.to_json f)))])
    let of_json j =
      {
        name =
          (Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json);
        creation_date =
          (Aws.Util.option_map (Aws.Json.lookup j "CreationDate")
             DateTime.of_json)
      }
  end
module Buckets =
  struct
    type t = Bucket.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map Bucket.parse (Aws.Xml.members "Bucket" xml))
    let to_query v = Aws.Query.to_query_list Bucket.to_query v
    let to_json v = `List (List.map Bucket.to_json v)
    let of_json j = Aws.Json.to_list Bucket.of_json j
  end
module ListBucketsOutput =
  struct
    type t = {
      buckets: Buckets.t ;
      owner: Owner.t option }
    let make ?(buckets= [])  ?owner  () = { buckets; owner }
    let parse xml =
      Some
        {
          buckets =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "Buckets" xml)
                  Buckets.parse));
          owner =
            (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) Owner.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.owner
              (fun f -> Aws.Query.Pair ("Owner", (Owner.to_query f)));
           Some
             (Aws.Query.Pair ("Buckets.member", (Buckets.to_query v.buckets)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.owner
              (fun f -> ("Owner", (Owner.to_json f)));
           Some ("Buckets", (Buckets.to_json v.buckets))])
    let of_json j =
      {
        buckets =
          (Buckets.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Buckets")));
        owner =
          (Aws.Util.option_map (Aws.Json.lookup j "Owner") Owner.of_json)
      }
  end
module GetBucketLifecycleConfigurationRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module ListBucketMetricsConfigurationsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      continuation_token: String.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?continuation_token  ?expected_bucket_owner  () =
      { bucket; continuation_token; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          continuation_token =
            (Aws.Util.option_bind (Aws.Xml.member "continuation-token" xml)
               String.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.continuation_token
             (fun f ->
                Aws.Query.Pair ("continuation-token", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.continuation_token
             (fun f -> ("continuation-token", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "continuation-token")
             String.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module InventoryConfigurationList =
  struct
    type t = InventoryConfiguration.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map InventoryConfiguration.parse (Aws.Xml.members "member" xml))
    let to_query v =
      Aws.Query.to_query_list InventoryConfiguration.to_query v
    let to_json v = `List (List.map InventoryConfiguration.to_json v)
    let of_json j = Aws.Json.to_list InventoryConfiguration.of_json j
  end
module DeleteBucketRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module IntelligentTieringConfigurationList =
  struct
    type t = IntelligentTieringConfiguration.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map IntelligentTieringConfiguration.parse
           (Aws.Xml.members "member" xml))
    let to_query v =
      Aws.Query.to_query_list IntelligentTieringConfiguration.to_query v
    let to_json v =
      `List (List.map IntelligentTieringConfiguration.to_json v)
    let of_json j =
      Aws.Json.to_list IntelligentTieringConfiguration.of_json j
  end
module GetBucketOwnershipControlsRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetBucketReplicationRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module AnalyticsAndOperator =
  struct
    type t = {
      prefix: String.t option ;
      tags: TagSet.t }
    let make ?prefix  ?(tags= [])  () = { prefix; tags }
    let parse xml =
      Some
        {
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          tags =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "Tag" xml) TagSet.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Tag", (TagSet.to_query v.tags)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Tag", (TagSet.to_json v.tags));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)))])
    let of_json j =
      {
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        tags =
          (TagSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tag")))
      }
  end
module AnalyticsFilter =
  struct
    type t =
      {
      prefix: String.t option ;
      tag: Tag.t option ;
      and_: AnalyticsAndOperator.t option }
    let make ?prefix  ?tag  ?and_  () = { prefix; tag; and_ }
    let parse xml =
      Some
        {
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          tag = (Aws.Util.option_bind (Aws.Xml.member "Tag" xml) Tag.parse);
          and_ =
            (Aws.Util.option_bind (Aws.Xml.member "And" xml)
               AnalyticsAndOperator.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.and_
              (fun f ->
                 Aws.Query.Pair ("And", (AnalyticsAndOperator.to_query f)));
           Aws.Util.option_map v.tag
             (fun f -> Aws.Query.Pair ("Tag", (Tag.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.and_
              (fun f -> ("And", (AnalyticsAndOperator.to_json f)));
           Aws.Util.option_map v.tag (fun f -> ("Tag", (Tag.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)))])
    let of_json j =
      {
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        tag = (Aws.Util.option_map (Aws.Json.lookup j "Tag") Tag.of_json);
        and_ =
          (Aws.Util.option_map (Aws.Json.lookup j "And")
             AnalyticsAndOperator.of_json)
      }
  end
module GetPublicAccessBlockRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module PolicyStatus =
  struct
    type t = {
      is_public: Boolean.t option }
    let make ?is_public  () = { is_public }
    let parse xml =
      Some
        {
          is_public =
            (Aws.Util.option_bind (Aws.Xml.member "IsPublic" xml)
               Boolean.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.is_public
              (fun f -> Aws.Query.Pair ("IsPublic", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.is_public
              (fun f -> ("IsPublic", (Boolean.to_json f)))])
    let of_json j =
      {
        is_public =
          (Aws.Util.option_map (Aws.Json.lookup j "IsPublic") Boolean.of_json)
      }
  end
module DeleteBucketPolicyRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module CompleteMultipartUploadRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      multipart_upload: CompletedMultipartUpload.t option ;
      upload_id: String.t ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option ;
      request_payer: RequestPayer.t option ;
      expected_bucket_owner: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option }
    let make ~bucket  ~key  ?multipart_upload  ~upload_id  ?checksum_c_r_c32 
      ?checksum_c_r_c32_c  ?checksum_s_h_a1  ?checksum_s_h_a256 
      ?request_payer  ?expected_bucket_owner  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key  ?s_s_e_customer_key_m_d5  () =
      {
        bucket;
        key;
        multipart_upload;
        upload_id;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256;
        request_payer;
        expected_bucket_owner;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          multipart_upload =
            (Aws.Util.option_bind
               (Aws.Xml.member "CompleteMultipartUpload" xml)
               CompletedMultipartUpload.parse);
          upload_id =
            (Aws.Xml.required "uploadId"
               (Aws.Util.option_bind (Aws.Xml.member "uploadId" xml)
                  String.parse));
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-crc32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-crc32c" xml) String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-sha1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-sha256" xml) String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-customer-key"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.s_s_e_customer_key_m_d5
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-server-side-encryption-customer-key-MD5",
                     (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32c", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32", (String.to_query f)));
           Some (Aws.Query.Pair ("uploadId", (String.to_query v.upload_id)));
           Aws.Util.option_map v.multipart_upload
             (fun f ->
                Aws.Query.Pair
                  ("CompleteMultipartUpload",
                    (CompletedMultipartUpload.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.s_s_e_customer_key_m_d5
              (fun f ->
                 ("x-amz-server-side-encryption-customer-key-MD5",
                   (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                ("x-amz-server-side-encryption-customer-key",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f -> ("x-amz-checksum-sha256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("x-amz-checksum-sha1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("x-amz-checksum-crc32c", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("x-amz-checksum-crc32", (String.to_json f)));
           Some ("uploadId", (String.to_json v.upload_id));
           Aws.Util.option_map v.multipart_upload
             (fun f ->
                ("CompleteMultipartUpload",
                  (CompletedMultipartUpload.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        multipart_upload =
          (Aws.Util.option_map (Aws.Json.lookup j "CompleteMultipartUpload")
             CompletedMultipartUpload.of_json);
        upload_id =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "uploadId")));
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32c")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha256")
             String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-customer-key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json)
      }
  end
module DeleteBucketAnalyticsConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      id: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~id  ?expected_bucket_owner  () =
      { bucket; id; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          id =
            (Aws.Xml.required "id"
               (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("id", (String.to_query v.id)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("id", (String.to_json v.id));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module UploadPartRequest =
  struct
    type t =
      {
      body: Blob.t option ;
      bucket: String.t ;
      content_length: Long.t option ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option ;
      key: String.t ;
      part_number: Integer.t ;
      upload_id: String.t ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      request_payer: RequestPayer.t option ;
      expected_bucket_owner: String.t option }
    let make ?body  ~bucket  ?content_length  ?content_m_d5 
      ?checksum_algorithm  ?checksum_c_r_c32  ?checksum_c_r_c32_c 
      ?checksum_s_h_a1  ?checksum_s_h_a256  ~key  ~part_number  ~upload_id 
      ?s_s_e_customer_algorithm  ?s_s_e_customer_key 
      ?s_s_e_customer_key_m_d5  ?request_payer  ?expected_bucket_owner  () =
      {
        body;
        bucket;
        content_length;
        content_m_d5;
        checksum_algorithm;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256;
        key;
        part_number;
        upload_id;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        request_payer;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          body =
            (Aws.Util.option_bind (Aws.Xml.member "Body" xml) Blob.parse);
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_length =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Length" xml)
               Long.parse);
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-crc32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-crc32c" xml) String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-sha1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-sha256" xml) String.parse);
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          part_number =
            (Aws.Xml.required "partNumber"
               (Aws.Util.option_bind (Aws.Xml.member "partNumber" xml)
                  Integer.parse));
          upload_id =
            (Aws.Xml.required "uploadId"
               (Aws.Util.option_bind (Aws.Xml.member "uploadId" xml)
                  String.parse));
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-customer-key"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Some (Aws.Query.Pair ("uploadId", (String.to_query v.upload_id)));
           Some
             (Aws.Query.Pair ("partNumber", (Integer.to_query v.part_number)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32c", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32", (String.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Aws.Util.option_map v.content_length
             (fun f -> Aws.Query.Pair ("Content-Length", (Long.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)));
           Aws.Util.option_map v.body
             (fun f -> Aws.Query.Pair ("Body", (Blob.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                ("x-amz-server-side-encryption-customer-key",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Some ("uploadId", (String.to_json v.upload_id));
           Some ("partNumber", (Integer.to_json v.part_number));
           Some ("Key", (String.to_json v.key));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f -> ("x-amz-checksum-sha256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("x-amz-checksum-sha1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("x-amz-checksum-crc32c", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("x-amz-checksum-crc32", (String.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Aws.Util.option_map v.content_length
             (fun f -> ("Content-Length", (Long.to_json f)));
           Some ("Bucket", (String.to_json v.bucket));
           Aws.Util.option_map v.body (fun f -> ("Body", (Blob.to_json f)))])
    let of_json j =
      {
        body = (Aws.Util.option_map (Aws.Json.lookup j "Body") Blob.of_json);
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_length =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Length")
             Long.of_json);
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32c")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha256")
             String.of_json);
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        part_number =
          (Integer.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "partNumber")));
        upload_id =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "uploadId")));
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-customer-key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module InvalidObjectState =
  struct
    type t =
      {
      storage_class: StorageClass.t option ;
      access_tier: IntelligentTieringAccessTier.t option }
    let make ?storage_class  ?access_tier  () =
      { storage_class; access_tier }
    let parse xml =
      Some
        {
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "StorageClass" xml)
               StorageClass.parse);
          access_tier =
            (Aws.Util.option_bind (Aws.Xml.member "AccessTier" xml)
               IntelligentTieringAccessTier.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.access_tier
              (fun f ->
                 Aws.Query.Pair
                   ("AccessTier", (IntelligentTieringAccessTier.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair ("StorageClass", (StorageClass.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.access_tier
              (fun f ->
                 ("AccessTier", (IntelligentTieringAccessTier.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f -> ("StorageClass", (StorageClass.to_json f)))])
    let of_json j =
      {
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "StorageClass")
             StorageClass.of_json);
        access_tier =
          (Aws.Util.option_map (Aws.Json.lookup j "AccessTier")
             IntelligentTieringAccessTier.of_json)
      }
  end
module BucketLoggingStatus =
  struct
    type t = {
      logging_enabled: LoggingEnabled.t option }
    let make ?logging_enabled  () = { logging_enabled }
    let parse xml =
      Some
        {
          logging_enabled =
            (Aws.Util.option_bind (Aws.Xml.member "LoggingEnabled" xml)
               LoggingEnabled.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.logging_enabled
              (fun f ->
                 Aws.Query.Pair
                   ("LoggingEnabled", (LoggingEnabled.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.logging_enabled
              (fun f -> ("LoggingEnabled", (LoggingEnabled.to_json f)))])
    let of_json j =
      {
        logging_enabled =
          (Aws.Util.option_map (Aws.Json.lookup j "LoggingEnabled")
             LoggingEnabled.of_json)
      }
  end
module PutBucketLoggingRequest =
  struct
    type t =
      {
      bucket: String.t ;
      bucket_logging_status: BucketLoggingStatus.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~bucket_logging_status  ?content_m_d5 
      ?checksum_algorithm  ?expected_bucket_owner  () =
      {
        bucket;
        bucket_logging_status;
        content_m_d5;
        checksum_algorithm;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          bucket_logging_status =
            (Aws.Xml.required "BucketLoggingStatus"
               (Aws.Util.option_bind
                  (Aws.Xml.member "BucketLoggingStatus" xml)
                  BucketLoggingStatus.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("BucketLoggingStatus",
                  (BucketLoggingStatus.to_query v.bucket_logging_status)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some
             ("BucketLoggingStatus",
               (BucketLoggingStatus.to_json v.bucket_logging_status));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        bucket_logging_status =
          (BucketLoggingStatus.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "BucketLoggingStatus")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetBucketMetricsConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      id: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~id  ?expected_bucket_owner  () =
      { bucket; id; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          id =
            (Aws.Xml.required "id"
               (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("id", (String.to_query v.id)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("id", (String.to_json v.id));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetBucketCorsOutput =
  struct
    type t = {
      c_o_r_s_rules: CORSRules.t }
    let make ?(c_o_r_s_rules= [])  () = { c_o_r_s_rules }
    let parse xml =
      Some { c_o_r_s_rules = (Aws.Util.of_option [] (CORSRules.parse xml)) }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("CORSRule", (CORSRules.to_query v.c_o_r_s_rules)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("CORSRule", (CORSRules.to_json v.c_o_r_s_rules))])
    let of_json j =
      {
        c_o_r_s_rules =
          (CORSRules.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "CORSRule")))
      }
  end
module ObjectAttributes =
  struct
    type t =
      | ETag 
      | Checksum 
      | ObjectParts 
      | StorageClass 
      | ObjectSize 
    let str_to_t =
      [("ObjectSize", ObjectSize);
      ("StorageClass", StorageClass);
      ("ObjectParts", ObjectParts);
      ("Checksum", Checksum);
      ("ETag", ETag)]
    let t_to_str =
      [(ObjectSize, "ObjectSize");
      (StorageClass, "StorageClass");
      (ObjectParts, "ObjectParts");
      (Checksum, "Checksum");
      (ETag, "ETag")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ObjectAttributesList =
  struct
    type t = ObjectAttributes.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map ObjectAttributes.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list ObjectAttributes.to_query v
    let to_json v = `List (List.map ObjectAttributes.to_json v)
    let of_json j = Aws.Json.to_list ObjectAttributes.of_json j
  end
module GetObjectAttributesRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      version_id: String.t option ;
      max_parts: Integer.t option ;
      part_number_marker: Integer.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      request_payer: RequestPayer.t option ;
      expected_bucket_owner: String.t option ;
      object_attributes: ObjectAttributesList.t }
    let make ~bucket  ~key  ?version_id  ?max_parts  ?part_number_marker 
      ?s_s_e_customer_algorithm  ?s_s_e_customer_key 
      ?s_s_e_customer_key_m_d5  ?request_payer  ?expected_bucket_owner 
      ~object_attributes  () =
      {
        bucket;
        key;
        version_id;
        max_parts;
        part_number_marker;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        request_payer;
        expected_bucket_owner;
        object_attributes
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          max_parts =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-max-parts" xml)
               Integer.parse);
          part_number_marker =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-part-number-marker" xml) Integer.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-customer-key"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          object_attributes =
            (Aws.Xml.required "x-amz-object-attributes"
               (Aws.Util.option_bind
                  (Aws.Xml.member "x-amz-object-attributes" xml)
                  ObjectAttributesList.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("x-amz-object-attributes",
                   (ObjectAttributesList.to_query v.object_attributes)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.part_number_marker
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-part-number-marker", (Integer.to_query f)));
           Aws.Util.option_map v.max_parts
             (fun f ->
                Aws.Query.Pair ("x-amz-max-parts", (Integer.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("x-amz-object-attributes",
                (ObjectAttributesList.to_json v.object_attributes));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                ("x-amz-server-side-encryption-customer-key",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.part_number_marker
             (fun f -> ("x-amz-part-number-marker", (Integer.to_json f)));
           Aws.Util.option_map v.max_parts
             (fun f -> ("x-amz-max-parts", (Integer.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        max_parts =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-max-parts")
             Integer.of_json);
        part_number_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-part-number-marker")
             Integer.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-customer-key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        object_attributes =
          (ObjectAttributesList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "x-amz-object-attributes")))
      }
  end
module ChecksumAlgorithmList =
  struct
    type t = ChecksumAlgorithm.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map ChecksumAlgorithm.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list ChecksumAlgorithm.to_query v
    let to_json v = `List (List.map ChecksumAlgorithm.to_json v)
    let of_json j = Aws.Json.to_list ChecksumAlgorithm.of_json j
  end
module ObjectLockEnabled =
  struct
    type t =
      | Enabled 
    let str_to_t = [("Enabled", Enabled)]
    let t_to_str = [(Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module ObjectLockConfiguration =
  struct
    type t =
      {
      object_lock_enabled: ObjectLockEnabled.t option ;
      rule: ObjectLockRule.t option }
    let make ?object_lock_enabled  ?rule  () = { object_lock_enabled; rule }
    let parse xml =
      Some
        {
          object_lock_enabled =
            (Aws.Util.option_bind (Aws.Xml.member "ObjectLockEnabled" xml)
               ObjectLockEnabled.parse);
          rule =
            (Aws.Util.option_bind (Aws.Xml.member "Rule" xml)
               ObjectLockRule.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.rule
              (fun f -> Aws.Query.Pair ("Rule", (ObjectLockRule.to_query f)));
           Aws.Util.option_map v.object_lock_enabled
             (fun f ->
                Aws.Query.Pair
                  ("ObjectLockEnabled", (ObjectLockEnabled.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.rule
              (fun f -> ("Rule", (ObjectLockRule.to_json f)));
           Aws.Util.option_map v.object_lock_enabled
             (fun f -> ("ObjectLockEnabled", (ObjectLockEnabled.to_json f)))])
    let of_json j =
      {
        object_lock_enabled =
          (Aws.Util.option_map (Aws.Json.lookup j "ObjectLockEnabled")
             ObjectLockEnabled.of_json);
        rule =
          (Aws.Util.option_map (Aws.Json.lookup j "Rule")
             ObjectLockRule.of_json)
      }
  end
module BucketLocationConstraint =
  struct
    type t =
      | Af_south_1 
      | Ap_east_1 
      | Ap_northeast_1 
      | Ap_northeast_2 
      | Ap_northeast_3 
      | Ap_south_1 
      | Ap_southeast_1 
      | Ap_southeast_2 
      | Ap_southeast_3 
      | Ca_central_1 
      | Cn_north_1 
      | Cn_northwest_1 
      | EU 
      | Eu_central_1 
      | Eu_north_1 
      | Eu_south_1 
      | Eu_west_1 
      | Eu_west_2 
      | Eu_west_3 
      | Me_south_1 
      | Sa_east_1 
      | Us_east_2 
      | Us_gov_east_1 
      | Us_gov_west_1 
      | Us_west_1 
      | Us_west_2 
    let str_to_t =
      [("us-west-2", Us_west_2);
      ("us-west-1", Us_west_1);
      ("us-gov-west-1", Us_gov_west_1);
      ("us-gov-east-1", Us_gov_east_1);
      ("us-east-2", Us_east_2);
      ("sa-east-1", Sa_east_1);
      ("me-south-1", Me_south_1);
      ("eu-west-3", Eu_west_3);
      ("eu-west-2", Eu_west_2);
      ("eu-west-1", Eu_west_1);
      ("eu-south-1", Eu_south_1);
      ("eu-north-1", Eu_north_1);
      ("eu-central-1", Eu_central_1);
      ("EU", EU);
      ("cn-northwest-1", Cn_northwest_1);
      ("cn-north-1", Cn_north_1);
      ("ca-central-1", Ca_central_1);
      ("ap-southeast-3", Ap_southeast_3);
      ("ap-southeast-2", Ap_southeast_2);
      ("ap-southeast-1", Ap_southeast_1);
      ("ap-south-1", Ap_south_1);
      ("ap-northeast-3", Ap_northeast_3);
      ("ap-northeast-2", Ap_northeast_2);
      ("ap-northeast-1", Ap_northeast_1);
      ("ap-east-1", Ap_east_1);
      ("af-south-1", Af_south_1)]
    let t_to_str =
      [(Us_west_2, "us-west-2");
      (Us_west_1, "us-west-1");
      (Us_gov_west_1, "us-gov-west-1");
      (Us_gov_east_1, "us-gov-east-1");
      (Us_east_2, "us-east-2");
      (Sa_east_1, "sa-east-1");
      (Me_south_1, "me-south-1");
      (Eu_west_3, "eu-west-3");
      (Eu_west_2, "eu-west-2");
      (Eu_west_1, "eu-west-1");
      (Eu_south_1, "eu-south-1");
      (Eu_north_1, "eu-north-1");
      (Eu_central_1, "eu-central-1");
      (EU, "EU");
      (Cn_northwest_1, "cn-northwest-1");
      (Cn_north_1, "cn-north-1");
      (Ca_central_1, "ca-central-1");
      (Ap_southeast_3, "ap-southeast-3");
      (Ap_southeast_2, "ap-southeast-2");
      (Ap_southeast_1, "ap-southeast-1");
      (Ap_south_1, "ap-south-1");
      (Ap_northeast_3, "ap-northeast-3");
      (Ap_northeast_2, "ap-northeast-2");
      (Ap_northeast_1, "ap-northeast-1");
      (Ap_east_1, "ap-east-1");
      (Af_south_1, "af-south-1")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module CreateBucketConfiguration =
  struct
    type t = {
      location_constraint: BucketLocationConstraint.t option }
    let make ?location_constraint  () = { location_constraint }
    let parse xml =
      Some
        {
          location_constraint =
            (Aws.Util.option_bind (Aws.Xml.member "LocationConstraint" xml)
               BucketLocationConstraint.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.location_constraint
              (fun f ->
                 Aws.Query.Pair
                   ("LocationConstraint",
                     (BucketLocationConstraint.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.location_constraint
              (fun f ->
                 ("LocationConstraint", (BucketLocationConstraint.to_json f)))])
    let of_json j =
      {
        location_constraint =
          (Aws.Util.option_map (Aws.Json.lookup j "LocationConstraint")
             BucketLocationConstraint.of_json)
      }
  end
module CreateBucketRequest =
  struct
    type t =
      {
      a_c_l: BucketCannedACL.t option ;
      bucket: String.t ;
      create_bucket_configuration: CreateBucketConfiguration.t option ;
      grant_full_control: String.t option ;
      grant_read: String.t option ;
      grant_read_a_c_p: String.t option ;
      grant_write: String.t option ;
      grant_write_a_c_p: String.t option ;
      object_lock_enabled_for_bucket: Boolean.t option ;
      object_ownership: ObjectOwnership.t option }
    let make ?a_c_l  ~bucket  ?create_bucket_configuration 
      ?grant_full_control  ?grant_read  ?grant_read_a_c_p  ?grant_write 
      ?grant_write_a_c_p  ?object_lock_enabled_for_bucket  ?object_ownership 
      () =
      {
        a_c_l;
        bucket;
        create_bucket_configuration;
        grant_full_control;
        grant_read;
        grant_read_a_c_p;
        grant_write;
        grant_write_a_c_p;
        object_lock_enabled_for_bucket;
        object_ownership
      }
    let parse xml =
      Some
        {
          a_c_l =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-acl" xml)
               BucketCannedACL.parse);
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          create_bucket_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "CreateBucketConfiguration" xml)
               CreateBucketConfiguration.parse);
          grant_full_control =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-grant-full-control" xml) String.parse);
          grant_read =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-read" xml)
               String.parse);
          grant_read_a_c_p =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-read-acp" xml)
               String.parse);
          grant_write =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-write" xml)
               String.parse);
          grant_write_a_c_p =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-grant-write-acp" xml) String.parse);
          object_lock_enabled_for_bucket =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-bucket-object-lock-enabled" xml)
               Boolean.parse);
          object_ownership =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-ownership" xml)
               ObjectOwnership.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.object_ownership
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-object-ownership", (ObjectOwnership.to_query f)));
           Aws.Util.option_map v.object_lock_enabled_for_bucket
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-bucket-object-lock-enabled", (Boolean.to_query f)));
           Aws.Util.option_map v.grant_write_a_c_p
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-write-acp", (String.to_query f)));
           Aws.Util.option_map v.grant_write
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-write", (String.to_query f)));
           Aws.Util.option_map v.grant_read_a_c_p
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-read-acp", (String.to_query f)));
           Aws.Util.option_map v.grant_read
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-read", (String.to_query f)));
           Aws.Util.option_map v.grant_full_control
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-grant-full-control", (String.to_query f)));
           Aws.Util.option_map v.create_bucket_configuration
             (fun f ->
                Aws.Query.Pair
                  ("CreateBucketConfiguration",
                    (CreateBucketConfiguration.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)));
           Aws.Util.option_map v.a_c_l
             (fun f ->
                Aws.Query.Pair ("x-amz-acl", (BucketCannedACL.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.object_ownership
              (fun f ->
                 ("x-amz-object-ownership", (ObjectOwnership.to_json f)));
           Aws.Util.option_map v.object_lock_enabled_for_bucket
             (fun f ->
                ("x-amz-bucket-object-lock-enabled", (Boolean.to_json f)));
           Aws.Util.option_map v.grant_write_a_c_p
             (fun f -> ("x-amz-grant-write-acp", (String.to_json f)));
           Aws.Util.option_map v.grant_write
             (fun f -> ("x-amz-grant-write", (String.to_json f)));
           Aws.Util.option_map v.grant_read_a_c_p
             (fun f -> ("x-amz-grant-read-acp", (String.to_json f)));
           Aws.Util.option_map v.grant_read
             (fun f -> ("x-amz-grant-read", (String.to_json f)));
           Aws.Util.option_map v.grant_full_control
             (fun f -> ("x-amz-grant-full-control", (String.to_json f)));
           Aws.Util.option_map v.create_bucket_configuration
             (fun f ->
                ("CreateBucketConfiguration",
                  (CreateBucketConfiguration.to_json f)));
           Some ("Bucket", (String.to_json v.bucket));
           Aws.Util.option_map v.a_c_l
             (fun f -> ("x-amz-acl", (BucketCannedACL.to_json f)))])
    let of_json j =
      {
        a_c_l =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-acl")
             BucketCannedACL.of_json);
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        create_bucket_configuration =
          (Aws.Util.option_map
             (Aws.Json.lookup j "CreateBucketConfiguration")
             CreateBucketConfiguration.of_json);
        grant_full_control =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-full-control")
             String.of_json);
        grant_read =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-read")
             String.of_json);
        grant_read_a_c_p =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-read-acp")
             String.of_json);
        grant_write =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-write")
             String.of_json);
        grant_write_a_c_p =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-write-acp")
             String.of_json);
        object_lock_enabled_for_bucket =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-bucket-object-lock-enabled")
             Boolean.of_json);
        object_ownership =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-object-ownership")
             ObjectOwnership.of_json)
      }
  end
module PutObjectAclRequest =
  struct
    type t =
      {
      a_c_l: ObjectCannedACL.t option ;
      access_control_policy: AccessControlPolicy.t option ;
      bucket: String.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      grant_full_control: String.t option ;
      grant_read: String.t option ;
      grant_read_a_c_p: String.t option ;
      grant_write: String.t option ;
      grant_write_a_c_p: String.t option ;
      key: String.t ;
      request_payer: RequestPayer.t option ;
      version_id: String.t option ;
      expected_bucket_owner: String.t option }
    let make ?a_c_l  ?access_control_policy  ~bucket  ?content_m_d5 
      ?checksum_algorithm  ?grant_full_control  ?grant_read 
      ?grant_read_a_c_p  ?grant_write  ?grant_write_a_c_p  ~key 
      ?request_payer  ?version_id  ?expected_bucket_owner  () =
      {
        a_c_l;
        access_control_policy;
        bucket;
        content_m_d5;
        checksum_algorithm;
        grant_full_control;
        grant_read;
        grant_read_a_c_p;
        grant_write;
        grant_write_a_c_p;
        key;
        request_payer;
        version_id;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          a_c_l =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-acl" xml)
               ObjectCannedACL.parse);
          access_control_policy =
            (Aws.Util.option_bind (Aws.Xml.member "AccessControlPolicy" xml)
               AccessControlPolicy.parse);
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          grant_full_control =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-grant-full-control" xml) String.parse);
          grant_read =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-read" xml)
               String.parse);
          grant_read_a_c_p =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-read-acp" xml)
               String.parse);
          grant_write =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-write" xml)
               String.parse);
          grant_write_a_c_p =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-grant-write-acp" xml) String.parse);
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Aws.Util.option_map v.grant_write_a_c_p
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-write-acp", (String.to_query f)));
           Aws.Util.option_map v.grant_write
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-write", (String.to_query f)));
           Aws.Util.option_map v.grant_read_a_c_p
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-read-acp", (String.to_query f)));
           Aws.Util.option_map v.grant_read
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-read", (String.to_query f)));
           Aws.Util.option_map v.grant_full_control
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-grant-full-control", (String.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)));
           Aws.Util.option_map v.access_control_policy
             (fun f ->
                Aws.Query.Pair
                  ("AccessControlPolicy", (AccessControlPolicy.to_query f)));
           Aws.Util.option_map v.a_c_l
             (fun f ->
                Aws.Query.Pair ("x-amz-acl", (ObjectCannedACL.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Some ("Key", (String.to_json v.key));
           Aws.Util.option_map v.grant_write_a_c_p
             (fun f -> ("x-amz-grant-write-acp", (String.to_json f)));
           Aws.Util.option_map v.grant_write
             (fun f -> ("x-amz-grant-write", (String.to_json f)));
           Aws.Util.option_map v.grant_read_a_c_p
             (fun f -> ("x-amz-grant-read-acp", (String.to_json f)));
           Aws.Util.option_map v.grant_read
             (fun f -> ("x-amz-grant-read", (String.to_json f)));
           Aws.Util.option_map v.grant_full_control
             (fun f -> ("x-amz-grant-full-control", (String.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket));
           Aws.Util.option_map v.access_control_policy
             (fun f ->
                ("AccessControlPolicy", (AccessControlPolicy.to_json f)));
           Aws.Util.option_map v.a_c_l
             (fun f -> ("x-amz-acl", (ObjectCannedACL.to_json f)))])
    let of_json j =
      {
        a_c_l =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-acl")
             ObjectCannedACL.of_json);
        access_control_policy =
          (Aws.Util.option_map (Aws.Json.lookup j "AccessControlPolicy")
             AccessControlPolicy.of_json);
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        grant_full_control =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-full-control")
             String.of_json);
        grant_read =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-read")
             String.of_json);
        grant_read_a_c_p =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-read-acp")
             String.of_json);
        grant_write =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-write")
             String.of_json);
        grant_write_a_c_p =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-write-acp")
             String.of_json);
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module MetricsStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)]
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module PublicAccessBlockConfiguration =
  struct
    type t =
      {
      block_public_acls: Boolean.t option ;
      ignore_public_acls: Boolean.t option ;
      block_public_policy: Boolean.t option ;
      restrict_public_buckets: Boolean.t option }
    let make ?block_public_acls  ?ignore_public_acls  ?block_public_policy 
      ?restrict_public_buckets  () =
      {
        block_public_acls;
        ignore_public_acls;
        block_public_policy;
        restrict_public_buckets
      }
    let parse xml =
      Some
        {
          block_public_acls =
            (Aws.Util.option_bind (Aws.Xml.member "BlockPublicAcls" xml)
               Boolean.parse);
          ignore_public_acls =
            (Aws.Util.option_bind (Aws.Xml.member "IgnorePublicAcls" xml)
               Boolean.parse);
          block_public_policy =
            (Aws.Util.option_bind (Aws.Xml.member "BlockPublicPolicy" xml)
               Boolean.parse);
          restrict_public_buckets =
            (Aws.Util.option_bind
               (Aws.Xml.member "RestrictPublicBuckets" xml) Boolean.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.restrict_public_buckets
              (fun f ->
                 Aws.Query.Pair
                   ("RestrictPublicBuckets", (Boolean.to_query f)));
           Aws.Util.option_map v.block_public_policy
             (fun f ->
                Aws.Query.Pair ("BlockPublicPolicy", (Boolean.to_query f)));
           Aws.Util.option_map v.ignore_public_acls
             (fun f ->
                Aws.Query.Pair ("IgnorePublicAcls", (Boolean.to_query f)));
           Aws.Util.option_map v.block_public_acls
             (fun f ->
                Aws.Query.Pair ("BlockPublicAcls", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.restrict_public_buckets
              (fun f -> ("RestrictPublicBuckets", (Boolean.to_json f)));
           Aws.Util.option_map v.block_public_policy
             (fun f -> ("BlockPublicPolicy", (Boolean.to_json f)));
           Aws.Util.option_map v.ignore_public_acls
             (fun f -> ("IgnorePublicAcls", (Boolean.to_json f)));
           Aws.Util.option_map v.block_public_acls
             (fun f -> ("BlockPublicAcls", (Boolean.to_json f)))])
    let of_json j =
      {
        block_public_acls =
          (Aws.Util.option_map (Aws.Json.lookup j "BlockPublicAcls")
             Boolean.of_json);
        ignore_public_acls =
          (Aws.Util.option_map (Aws.Json.lookup j "IgnorePublicAcls")
             Boolean.of_json);
        block_public_policy =
          (Aws.Util.option_map (Aws.Json.lookup j "BlockPublicPolicy")
             Boolean.of_json);
        restrict_public_buckets =
          (Aws.Util.option_map (Aws.Json.lookup j "RestrictPublicBuckets")
             Boolean.of_json)
      }
  end
module GetObjectTorrentOutput =
  struct
    type t = {
      body: Blob.t option ;
      request_charged: RequestCharged.t option }
    let make ?body  ?request_charged  () = { body; request_charged }
    let parse xml =
      Some
        {
          body =
            (Aws.Util.option_bind (Aws.Xml.member "Body" xml) Blob.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.body
             (fun f -> Aws.Query.Pair ("Body", (Blob.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.body (fun f -> ("Body", (Blob.to_json f)))])
    let of_json j =
      {
        body = (Aws.Util.option_map (Aws.Json.lookup j "Body") Blob.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module DeleteBucketInventoryConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      id: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~id  ?expected_bucket_owner  () =
      { bucket; id; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          id =
            (Aws.Xml.required "id"
               (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("id", (String.to_query v.id)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("id", (String.to_json v.id));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module ReplicationRuleFilter =
  struct
    type t =
      {
      prefix: String.t option ;
      tag: Tag.t option ;
      and_: ReplicationRuleAndOperator.t option }
    let make ?prefix  ?tag  ?and_  () = { prefix; tag; and_ }
    let parse xml =
      Some
        {
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          tag = (Aws.Util.option_bind (Aws.Xml.member "Tag" xml) Tag.parse);
          and_ =
            (Aws.Util.option_bind (Aws.Xml.member "And" xml)
               ReplicationRuleAndOperator.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.and_
              (fun f ->
                 Aws.Query.Pair
                   ("And", (ReplicationRuleAndOperator.to_query f)));
           Aws.Util.option_map v.tag
             (fun f -> Aws.Query.Pair ("Tag", (Tag.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.and_
              (fun f -> ("And", (ReplicationRuleAndOperator.to_json f)));
           Aws.Util.option_map v.tag (fun f -> ("Tag", (Tag.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)))])
    let of_json j =
      {
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        tag = (Aws.Util.option_map (Aws.Json.lookup j "Tag") Tag.of_json);
        and_ =
          (Aws.Util.option_map (Aws.Json.lookup j "And")
             ReplicationRuleAndOperator.of_json)
      }
  end
module GetObjectRetentionOutput =
  struct
    type t = {
      retention: ObjectLockRetention.t option }
    let make ?retention  () = { retention }
    let parse xml =
      Some
        {
          retention =
            (Aws.Util.option_bind (Aws.Xml.member "Retention" xml)
               ObjectLockRetention.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.retention
              (fun f ->
                 Aws.Query.Pair
                   ("Retention", (ObjectLockRetention.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.retention
              (fun f -> ("Retention", (ObjectLockRetention.to_json f)))])
    let of_json j =
      {
        retention =
          (Aws.Util.option_map (Aws.Json.lookup j "Retention")
             ObjectLockRetention.of_json)
      }
  end
module MetricsConfigurationList =
  struct
    type t = MetricsConfiguration.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map MetricsConfiguration.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list MetricsConfiguration.to_query v
    let to_json v = `List (List.map MetricsConfiguration.to_json v)
    let of_json j = Aws.Json.to_list MetricsConfiguration.of_json j
  end
module ListBucketMetricsConfigurationsOutput =
  struct
    type t =
      {
      is_truncated: Boolean.t option ;
      continuation_token: String.t option ;
      next_continuation_token: String.t option ;
      metrics_configuration_list: MetricsConfigurationList.t }
    let make ?is_truncated  ?continuation_token  ?next_continuation_token 
      ?(metrics_configuration_list= [])  () =
      {
        is_truncated;
        continuation_token;
        next_continuation_token;
        metrics_configuration_list
      }
    let parse xml =
      Some
        {
          is_truncated =
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml)
               Boolean.parse);
          continuation_token =
            (Aws.Util.option_bind (Aws.Xml.member "ContinuationToken" xml)
               String.parse);
          next_continuation_token =
            (Aws.Util.option_bind
               (Aws.Xml.member "NextContinuationToken" xml) String.parse);
          metrics_configuration_list =
            (Aws.Util.of_option [] (MetricsConfigurationList.parse xml))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("MetricsConfiguration",
                   (MetricsConfigurationList.to_query
                      v.metrics_configuration_list)));
           Aws.Util.option_map v.next_continuation_token
             (fun f ->
                Aws.Query.Pair ("NextContinuationToken", (String.to_query f)));
           Aws.Util.option_map v.continuation_token
             (fun f ->
                Aws.Query.Pair ("ContinuationToken", (String.to_query f)));
           Aws.Util.option_map v.is_truncated
             (fun f -> Aws.Query.Pair ("IsTruncated", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("MetricsConfiguration",
                (MetricsConfigurationList.to_json
                   v.metrics_configuration_list));
           Aws.Util.option_map v.next_continuation_token
             (fun f -> ("NextContinuationToken", (String.to_json f)));
           Aws.Util.option_map v.continuation_token
             (fun f -> ("ContinuationToken", (String.to_json f)));
           Aws.Util.option_map v.is_truncated
             (fun f -> ("IsTruncated", (Boolean.to_json f)))])
    let of_json j =
      {
        is_truncated =
          (Aws.Util.option_map (Aws.Json.lookup j "IsTruncated")
             Boolean.of_json);
        continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "ContinuationToken")
             String.of_json);
        next_continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "NextContinuationToken")
             String.of_json);
        metrics_configuration_list =
          (MetricsConfigurationList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "MetricsConfiguration")))
      }
  end
module GetBucketIntelligentTieringConfigurationRequest =
  struct
    type t = {
      bucket: String.t ;
      id: String.t }
    let make ~bucket  ~id  () = { bucket; id }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          id =
            (Aws.Xml.required "id"
               (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("id", (String.to_query v.id)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("id", (String.to_json v.id));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id")))
      }
  end
module PutObjectAclOutput =
  struct
    type t = {
      request_charged: RequestCharged.t option }
    let make ?request_charged  () = { request_charged }
    let parse xml =
      Some
        {
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)))])
    let of_json j =
      {
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module PutBucketAccelerateConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      accelerate_configuration: AccelerateConfiguration.t ;
      expected_bucket_owner: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option }
    let make ~bucket  ~accelerate_configuration  ?expected_bucket_owner 
      ?checksum_algorithm  () =
      {
        bucket;
        accelerate_configuration;
        expected_bucket_owner;
        checksum_algorithm
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          accelerate_configuration =
            (Aws.Xml.required "AccelerateConfiguration"
               (Aws.Util.option_bind
                  (Aws.Xml.member "AccelerateConfiguration" xml)
                  AccelerateConfiguration.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_algorithm
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-sdk-checksum-algorithm",
                     (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("AccelerateConfiguration",
                  (AccelerateConfiguration.to_query
                     v.accelerate_configuration)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_algorithm
              (fun f ->
                 ("x-amz-sdk-checksum-algorithm",
                   (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some
             ("AccelerateConfiguration",
               (AccelerateConfiguration.to_json v.accelerate_configuration));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        accelerate_configuration =
          (AccelerateConfiguration.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "AccelerateConfiguration")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json)
      }
  end
module PutPublicAccessBlockRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      public_access_block_configuration: PublicAccessBlockConfiguration.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?content_m_d5  ?checksum_algorithm 
      ~public_access_block_configuration  ?expected_bucket_owner  () =
      {
        bucket;
        content_m_d5;
        checksum_algorithm;
        public_access_block_configuration;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          public_access_block_configuration =
            (Aws.Xml.required "PublicAccessBlockConfiguration"
               (Aws.Util.option_bind
                  (Aws.Xml.member "PublicAccessBlockConfiguration" xml)
                  PublicAccessBlockConfiguration.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("PublicAccessBlockConfiguration",
                  (PublicAccessBlockConfiguration.to_query
                     v.public_access_block_configuration)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some
             ("PublicAccessBlockConfiguration",
               (PublicAccessBlockConfiguration.to_json
                  v.public_access_block_configuration));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        public_access_block_configuration =
          (PublicAccessBlockConfiguration.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "PublicAccessBlockConfiguration")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module ListBucketIntelligentTieringConfigurationsRequest =
  struct
    type t = {
      bucket: String.t ;
      continuation_token: String.t option }
    let make ~bucket  ?continuation_token  () =
      { bucket; continuation_token }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          continuation_token =
            (Aws.Util.option_bind (Aws.Xml.member "continuation-token" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.continuation_token
              (fun f ->
                 Aws.Query.Pair ("continuation-token", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.continuation_token
              (fun f -> ("continuation-token", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "continuation-token")
             String.of_json)
      }
  end
module DeleteObjectTaggingRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      version_id: String.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~key  ?version_id  ?expected_bucket_owner  () =
      { bucket; key; version_id; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module PutObjectLegalHoldRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      legal_hold: ObjectLockLegalHold.t option ;
      request_payer: RequestPayer.t option ;
      version_id: String.t option ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~key  ?legal_hold  ?request_payer  ?version_id 
      ?content_m_d5  ?checksum_algorithm  ?expected_bucket_owner  () =
      {
        bucket;
        key;
        legal_hold;
        request_payer;
        version_id;
        content_m_d5;
        checksum_algorithm;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          legal_hold =
            (Aws.Util.option_bind (Aws.Xml.member "LegalHold" xml)
               ObjectLockLegalHold.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.legal_hold
             (fun f ->
                Aws.Query.Pair
                  ("LegalHold", (ObjectLockLegalHold.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.legal_hold
             (fun f -> ("LegalHold", (ObjectLockLegalHold.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        legal_hold =
          (Aws.Util.option_map (Aws.Json.lookup j "LegalHold")
             ObjectLockLegalHold.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module ArchiveStatus =
  struct
    type t =
      | ARCHIVE_ACCESS 
      | DEEP_ARCHIVE_ACCESS 
    let str_to_t =
      [("DEEP_ARCHIVE_ACCESS", DEEP_ARCHIVE_ACCESS);
      ("ARCHIVE_ACCESS", ARCHIVE_ACCESS)]
    let t_to_str =
      [(DEEP_ARCHIVE_ACCESS, "DEEP_ARCHIVE_ACCESS");
      (ARCHIVE_ACCESS, "ARCHIVE_ACCESS")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module HeadObjectOutput =
  struct
    type t =
      {
      delete_marker: Boolean.t option ;
      accept_ranges: String.t option ;
      expiration: String.t option ;
      restore: String.t option ;
      archive_status: ArchiveStatus.t option ;
      last_modified: DateTime.t option ;
      content_length: Long.t option ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option ;
      e_tag: String.t option ;
      missing_meta: Integer.t option ;
      version_id: String.t option ;
      cache_control: String.t option ;
      content_disposition: String.t option ;
      content_encoding: String.t option ;
      content_language: String.t option ;
      content_type: String.t option ;
      expires: DateTime.t option ;
      website_redirect_location: String.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      metadata: Metadata.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      bucket_key_enabled: Boolean.t option ;
      storage_class: StorageClass.t option ;
      request_charged: RequestCharged.t option ;
      replication_status: ReplicationStatus.t option ;
      parts_count: Integer.t option ;
      object_lock_mode: ObjectLockMode.t option ;
      object_lock_retain_until_date: DateTime.t option ;
      object_lock_legal_hold_status: ObjectLockLegalHoldStatus.t option }
    let make ?delete_marker  ?accept_ranges  ?expiration  ?restore 
      ?archive_status  ?last_modified  ?content_length  ?checksum_c_r_c32 
      ?checksum_c_r_c32_c  ?checksum_s_h_a1  ?checksum_s_h_a256  ?e_tag 
      ?missing_meta  ?version_id  ?cache_control  ?content_disposition 
      ?content_encoding  ?content_language  ?content_type  ?expires 
      ?website_redirect_location  ?server_side_encryption  ?metadata 
      ?s_s_e_customer_algorithm  ?s_s_e_customer_key_m_d5 
      ?s_s_e_k_m_s_key_id  ?bucket_key_enabled  ?storage_class 
      ?request_charged  ?replication_status  ?parts_count  ?object_lock_mode 
      ?object_lock_retain_until_date  ?object_lock_legal_hold_status  () =
      {
        delete_marker;
        accept_ranges;
        expiration;
        restore;
        archive_status;
        last_modified;
        content_length;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256;
        e_tag;
        missing_meta;
        version_id;
        cache_control;
        content_disposition;
        content_encoding;
        content_language;
        content_type;
        expires;
        website_redirect_location;
        server_side_encryption;
        metadata;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        bucket_key_enabled;
        storage_class;
        request_charged;
        replication_status;
        parts_count;
        object_lock_mode;
        object_lock_retain_until_date;
        object_lock_legal_hold_status
      }
    let parse xml =
      Some
        {
          delete_marker =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-delete-marker" xml)
               Boolean.parse);
          accept_ranges =
            (Aws.Util.option_bind (Aws.Xml.member "accept-ranges" xml)
               String.parse);
          expiration =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-expiration" xml)
               String.parse);
          restore =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-restore" xml)
               String.parse);
          archive_status =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-archive-status" xml)
               ArchiveStatus.parse);
          last_modified =
            (Aws.Util.option_bind (Aws.Xml.member "Last-Modified" xml)
               DateTime.parse);
          content_length =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Length" xml)
               Long.parse);
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-crc32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-crc32c" xml) String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-sha1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-sha256" xml) String.parse);
          e_tag =
            (Aws.Util.option_bind (Aws.Xml.member "ETag" xml) String.parse);
          missing_meta =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-missing-meta" xml)
               Integer.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-version-id" xml)
               String.parse);
          cache_control =
            (Aws.Util.option_bind (Aws.Xml.member "Cache-Control" xml)
               String.parse);
          content_disposition =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Disposition" xml)
               String.parse);
          content_encoding =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Encoding" xml)
               String.parse);
          content_language =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Language" xml)
               String.parse);
          content_type =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Type" xml)
               String.parse);
          expires =
            (Aws.Util.option_bind (Aws.Xml.member "Expires" xml)
               DateTime.parse);
          website_redirect_location =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-website-redirect-location" xml)
               String.parse);
          server_side_encryption =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          metadata =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-meta-" xml)
               Metadata.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          s_s_e_k_m_s_key_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-aws-kms-key-id"
                  xml) String.parse);
          bucket_key_enabled =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-bucket-key-enabled" xml)
               Boolean.parse);
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-storage-class" xml)
               StorageClass.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse);
          replication_status =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-replication-status" xml)
               ReplicationStatus.parse);
          parts_count =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-mp-parts-count" xml)
               Integer.parse);
          object_lock_mode =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-mode" xml)
               ObjectLockMode.parse);
          object_lock_retain_until_date =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-retain-until-date" xml)
               DateTime.parse);
          object_lock_legal_hold_status =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-legal-hold" xml)
               ObjectLockLegalHoldStatus.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.object_lock_legal_hold_status
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-object-lock-legal-hold",
                     (ObjectLockLegalHoldStatus.to_query f)));
           Aws.Util.option_map v.object_lock_retain_until_date
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-object-lock-retain-until-date",
                    (DateTime.to_query f)));
           Aws.Util.option_map v.object_lock_mode
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-object-lock-mode", (ObjectLockMode.to_query f)));
           Aws.Util.option_map v.parts_count
             (fun f ->
                Aws.Query.Pair ("x-amz-mp-parts-count", (Integer.to_query f)));
           Aws.Util.option_map v.replication_status
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-replication-status",
                    (ReplicationStatus.to_query f)));
           Aws.Util.option_map v.request_charged
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-storage-class", (StorageClass.to_query f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-bucket-key-enabled",
                    (Boolean.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.metadata
             (fun f -> Aws.Query.Pair ("x-amz-meta-", (Metadata.to_query f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Aws.Util.option_map v.website_redirect_location
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-website-redirect-location", (String.to_query f)));
           Aws.Util.option_map v.expires
             (fun f -> Aws.Query.Pair ("Expires", (DateTime.to_query f)));
           Aws.Util.option_map v.content_type
             (fun f -> Aws.Query.Pair ("Content-Type", (String.to_query f)));
           Aws.Util.option_map v.content_language
             (fun f ->
                Aws.Query.Pair ("Content-Language", (String.to_query f)));
           Aws.Util.option_map v.content_encoding
             (fun f ->
                Aws.Query.Pair ("Content-Encoding", (String.to_query f)));
           Aws.Util.option_map v.content_disposition
             (fun f ->
                Aws.Query.Pair ("Content-Disposition", (String.to_query f)));
           Aws.Util.option_map v.cache_control
             (fun f -> Aws.Query.Pair ("Cache-Control", (String.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f ->
                Aws.Query.Pair ("x-amz-version-id", (String.to_query f)));
           Aws.Util.option_map v.missing_meta
             (fun f ->
                Aws.Query.Pair ("x-amz-missing-meta", (Integer.to_query f)));
           Aws.Util.option_map v.e_tag
             (fun f -> Aws.Query.Pair ("ETag", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32c", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32", (String.to_query f)));
           Aws.Util.option_map v.content_length
             (fun f -> Aws.Query.Pair ("Content-Length", (Long.to_query f)));
           Aws.Util.option_map v.last_modified
             (fun f ->
                Aws.Query.Pair ("Last-Modified", (DateTime.to_query f)));
           Aws.Util.option_map v.archive_status
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-archive-status", (ArchiveStatus.to_query f)));
           Aws.Util.option_map v.restore
             (fun f -> Aws.Query.Pair ("x-amz-restore", (String.to_query f)));
           Aws.Util.option_map v.expiration
             (fun f ->
                Aws.Query.Pair ("x-amz-expiration", (String.to_query f)));
           Aws.Util.option_map v.accept_ranges
             (fun f -> Aws.Query.Pair ("accept-ranges", (String.to_query f)));
           Aws.Util.option_map v.delete_marker
             (fun f ->
                Aws.Query.Pair ("x-amz-delete-marker", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.object_lock_legal_hold_status
              (fun f ->
                 ("x-amz-object-lock-legal-hold",
                   (ObjectLockLegalHoldStatus.to_json f)));
           Aws.Util.option_map v.object_lock_retain_until_date
             (fun f ->
                ("x-amz-object-lock-retain-until-date", (DateTime.to_json f)));
           Aws.Util.option_map v.object_lock_mode
             (fun f -> ("x-amz-object-lock-mode", (ObjectLockMode.to_json f)));
           Aws.Util.option_map v.parts_count
             (fun f -> ("x-amz-mp-parts-count", (Integer.to_json f)));
           Aws.Util.option_map v.replication_status
             (fun f ->
                ("x-amz-replication-status", (ReplicationStatus.to_json f)));
           Aws.Util.option_map v.request_charged
             (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f -> ("x-amz-storage-class", (StorageClass.to_json f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                ("x-amz-server-side-encryption-bucket-key-enabled",
                  (Boolean.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                ("x-amz-server-side-encryption-aws-kms-key-id",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.metadata
             (fun f -> ("x-amz-meta-", (Metadata.to_json f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                ("x-amz-server-side-encryption",
                  (ServerSideEncryption.to_json f)));
           Aws.Util.option_map v.website_redirect_location
             (fun f ->
                ("x-amz-website-redirect-location", (String.to_json f)));
           Aws.Util.option_map v.expires
             (fun f -> ("Expires", (DateTime.to_json f)));
           Aws.Util.option_map v.content_type
             (fun f -> ("Content-Type", (String.to_json f)));
           Aws.Util.option_map v.content_language
             (fun f -> ("Content-Language", (String.to_json f)));
           Aws.Util.option_map v.content_encoding
             (fun f -> ("Content-Encoding", (String.to_json f)));
           Aws.Util.option_map v.content_disposition
             (fun f -> ("Content-Disposition", (String.to_json f)));
           Aws.Util.option_map v.cache_control
             (fun f -> ("Cache-Control", (String.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("x-amz-version-id", (String.to_json f)));
           Aws.Util.option_map v.missing_meta
             (fun f -> ("x-amz-missing-meta", (Integer.to_json f)));
           Aws.Util.option_map v.e_tag
             (fun f -> ("ETag", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f -> ("x-amz-checksum-sha256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("x-amz-checksum-sha1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("x-amz-checksum-crc32c", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("x-amz-checksum-crc32", (String.to_json f)));
           Aws.Util.option_map v.content_length
             (fun f -> ("Content-Length", (Long.to_json f)));
           Aws.Util.option_map v.last_modified
             (fun f -> ("Last-Modified", (DateTime.to_json f)));
           Aws.Util.option_map v.archive_status
             (fun f -> ("x-amz-archive-status", (ArchiveStatus.to_json f)));
           Aws.Util.option_map v.restore
             (fun f -> ("x-amz-restore", (String.to_json f)));
           Aws.Util.option_map v.expiration
             (fun f -> ("x-amz-expiration", (String.to_json f)));
           Aws.Util.option_map v.accept_ranges
             (fun f -> ("accept-ranges", (String.to_json f)));
           Aws.Util.option_map v.delete_marker
             (fun f -> ("x-amz-delete-marker", (Boolean.to_json f)))])
    let of_json j =
      {
        delete_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-delete-marker")
             Boolean.of_json);
        accept_ranges =
          (Aws.Util.option_map (Aws.Json.lookup j "accept-ranges")
             String.of_json);
        expiration =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-expiration")
             String.of_json);
        restore =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-restore")
             String.of_json);
        archive_status =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-archive-status")
             ArchiveStatus.of_json);
        last_modified =
          (Aws.Util.option_map (Aws.Json.lookup j "Last-Modified")
             DateTime.of_json);
        content_length =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Length")
             Long.of_json);
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32c")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha256")
             String.of_json);
        e_tag =
          (Aws.Util.option_map (Aws.Json.lookup j "ETag") String.of_json);
        missing_meta =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-missing-meta")
             Integer.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-version-id")
             String.of_json);
        cache_control =
          (Aws.Util.option_map (Aws.Json.lookup j "Cache-Control")
             String.of_json);
        content_disposition =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Disposition")
             String.of_json);
        content_encoding =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Encoding")
             String.of_json);
        content_language =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Language")
             String.of_json);
        content_type =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Type")
             String.of_json);
        expires =
          (Aws.Util.option_map (Aws.Json.lookup j "Expires") DateTime.of_json);
        website_redirect_location =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-website-redirect-location")
             String.of_json);
        server_side_encryption =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption")
             ServerSideEncryption.of_json);
        metadata =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-meta-")
             Metadata.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-aws-kms-key-id")
             String.of_json);
        bucket_key_enabled =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-bucket-key-enabled")
             Boolean.of_json);
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-storage-class")
             StorageClass.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json);
        replication_status =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-replication-status")
             ReplicationStatus.of_json);
        parts_count =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-mp-parts-count")
             Integer.of_json);
        object_lock_mode =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-object-lock-mode")
             ObjectLockMode.of_json);
        object_lock_retain_until_date =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-object-lock-retain-until-date")
             DateTime.of_json);
        object_lock_legal_hold_status =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-object-lock-legal-hold")
             ObjectLockLegalHoldStatus.of_json)
      }
  end
module ServerSideEncryptionRule =
  struct
    type t =
      {
      apply_server_side_encryption_by_default:
        ServerSideEncryptionByDefault.t option ;
      bucket_key_enabled: Boolean.t option }
    let make ?apply_server_side_encryption_by_default  ?bucket_key_enabled 
      () = { apply_server_side_encryption_by_default; bucket_key_enabled }
    let parse xml =
      Some
        {
          apply_server_side_encryption_by_default =
            (Aws.Util.option_bind
               (Aws.Xml.member "ApplyServerSideEncryptionByDefault" xml)
               ServerSideEncryptionByDefault.parse);
          bucket_key_enabled =
            (Aws.Util.option_bind (Aws.Xml.member "BucketKeyEnabled" xml)
               Boolean.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.bucket_key_enabled
              (fun f ->
                 Aws.Query.Pair ("BucketKeyEnabled", (Boolean.to_query f)));
           Aws.Util.option_map v.apply_server_side_encryption_by_default
             (fun f ->
                Aws.Query.Pair
                  ("ApplyServerSideEncryptionByDefault",
                    (ServerSideEncryptionByDefault.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.bucket_key_enabled
              (fun f -> ("BucketKeyEnabled", (Boolean.to_json f)));
           Aws.Util.option_map v.apply_server_side_encryption_by_default
             (fun f ->
                ("ApplyServerSideEncryptionByDefault",
                  (ServerSideEncryptionByDefault.to_json f)))])
    let of_json j =
      {
        apply_server_side_encryption_by_default =
          (Aws.Util.option_map
             (Aws.Json.lookup j "ApplyServerSideEncryptionByDefault")
             ServerSideEncryptionByDefault.of_json);
        bucket_key_enabled =
          (Aws.Util.option_map (Aws.Json.lookup j "BucketKeyEnabled")
             Boolean.of_json)
      }
  end
module ServerSideEncryptionRules =
  struct
    type t = ServerSideEncryptionRule.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map ServerSideEncryptionRule.parse
           (Aws.Xml.members "member" xml))
    let to_query v =
      Aws.Query.to_query_list ServerSideEncryptionRule.to_query v
    let to_json v = `List (List.map ServerSideEncryptionRule.to_json v)
    let of_json j = Aws.Json.to_list ServerSideEncryptionRule.of_json j
  end
module ServerSideEncryptionConfiguration =
  struct
    type t = {
      rules: ServerSideEncryptionRules.t }
    let make ~rules  () = { rules }
    let parse xml =
      Some
        {
          rules =
            (Aws.Xml.required "Rule" (ServerSideEncryptionRules.parse xml))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("Rule", (ServerSideEncryptionRules.to_query v.rules)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Rule", (ServerSideEncryptionRules.to_json v.rules))])
    let of_json j =
      {
        rules =
          (ServerSideEncryptionRules.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Rule")))
      }
  end
module GetBucketEncryptionOutput =
  struct
    type t =
      {
      server_side_encryption_configuration:
        ServerSideEncryptionConfiguration.t option }
    let make ?server_side_encryption_configuration  () =
      { server_side_encryption_configuration }
    let parse xml =
      Some
        {
          server_side_encryption_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "ServerSideEncryptionConfiguration" xml)
               ServerSideEncryptionConfiguration.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.server_side_encryption_configuration
              (fun f ->
                 Aws.Query.Pair
                   ("ServerSideEncryptionConfiguration",
                     (ServerSideEncryptionConfiguration.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.server_side_encryption_configuration
              (fun f ->
                 ("ServerSideEncryptionConfiguration",
                   (ServerSideEncryptionConfiguration.to_json f)))])
    let of_json j =
      {
        server_side_encryption_configuration =
          (Aws.Util.option_map
             (Aws.Json.lookup j "ServerSideEncryptionConfiguration")
             ServerSideEncryptionConfiguration.of_json)
      }
  end
module RestoreStatus =
  struct
    type t =
      {
      is_restore_in_progress: Boolean.t option ;
      restore_expiry_date: DateTime.t option }
    let make ?is_restore_in_progress  ?restore_expiry_date  () =
      { is_restore_in_progress; restore_expiry_date }
    let parse xml =
      Some
        {
          is_restore_in_progress =
            (Aws.Util.option_bind (Aws.Xml.member "IsRestoreInProgress" xml)
               Boolean.parse);
          restore_expiry_date =
            (Aws.Util.option_bind (Aws.Xml.member "RestoreExpiryDate" xml)
               DateTime.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.restore_expiry_date
              (fun f ->
                 Aws.Query.Pair ("RestoreExpiryDate", (DateTime.to_query f)));
           Aws.Util.option_map v.is_restore_in_progress
             (fun f ->
                Aws.Query.Pair ("IsRestoreInProgress", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.restore_expiry_date
              (fun f -> ("RestoreExpiryDate", (DateTime.to_json f)));
           Aws.Util.option_map v.is_restore_in_progress
             (fun f -> ("IsRestoreInProgress", (Boolean.to_json f)))])
    let of_json j =
      {
        is_restore_in_progress =
          (Aws.Util.option_map (Aws.Json.lookup j "IsRestoreInProgress")
             Boolean.of_json);
        restore_expiry_date =
          (Aws.Util.option_map (Aws.Json.lookup j "RestoreExpiryDate")
             DateTime.of_json)
      }
  end
module ObjectStorageClass =
  struct
    type t =
      | STANDARD 
      | REDUCED_REDUNDANCY 
      | GLACIER 
      | STANDARD_IA 
      | ONEZONE_IA 
      | INTELLIGENT_TIERING 
      | DEEP_ARCHIVE 
      | OUTPOSTS 
      | GLACIER_IR 
      | SNOW 
    let str_to_t =
      [("SNOW", SNOW);
      ("GLACIER_IR", GLACIER_IR);
      ("OUTPOSTS", OUTPOSTS);
      ("DEEP_ARCHIVE", DEEP_ARCHIVE);
      ("INTELLIGENT_TIERING", INTELLIGENT_TIERING);
      ("ONEZONE_IA", ONEZONE_IA);
      ("STANDARD_IA", STANDARD_IA);
      ("GLACIER", GLACIER);
      ("REDUCED_REDUNDANCY", REDUCED_REDUNDANCY);
      ("STANDARD", STANDARD)]
    let t_to_str =
      [(SNOW, "SNOW");
      (GLACIER_IR, "GLACIER_IR");
      (OUTPOSTS, "OUTPOSTS");
      (DEEP_ARCHIVE, "DEEP_ARCHIVE");
      (INTELLIGENT_TIERING, "INTELLIGENT_TIERING");
      (ONEZONE_IA, "ONEZONE_IA");
      (STANDARD_IA, "STANDARD_IA");
      (GLACIER, "GLACIER");
      (REDUCED_REDUNDANCY, "REDUCED_REDUNDANCY");
      (STANDARD, "STANDARD")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module Object =
  struct
    type t =
      {
      key: String.t option ;
      last_modified: DateTime.t option ;
      e_tag: String.t option ;
      checksum_algorithm: ChecksumAlgorithmList.t ;
      size: Integer.t option ;
      storage_class: ObjectStorageClass.t option ;
      owner: Owner.t option ;
      restore_status: RestoreStatus.t option }
    let make ?key  ?last_modified  ?e_tag  ?(checksum_algorithm= [])  ?size 
      ?storage_class  ?owner  ?restore_status  () =
      {
        key;
        last_modified;
        e_tag;
        checksum_algorithm;
        size;
        storage_class;
        owner;
        restore_status
      }
    let parse xml =
      Some
        {
          key =
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse);
          last_modified =
            (Aws.Util.option_bind (Aws.Xml.member "LastModified" xml)
               DateTime.parse);
          e_tag =
            (Aws.Util.option_bind (Aws.Xml.member "ETag" xml) String.parse);
          checksum_algorithm =
            (Aws.Util.of_option [] (ChecksumAlgorithmList.parse xml));
          size =
            (Aws.Util.option_bind (Aws.Xml.member "Size" xml) Integer.parse);
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "StorageClass" xml)
               ObjectStorageClass.parse);
          owner =
            (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) Owner.parse);
          restore_status =
            (Aws.Util.option_bind (Aws.Xml.member "RestoreStatus" xml)
               RestoreStatus.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.restore_status
              (fun f ->
                 Aws.Query.Pair ("RestoreStatus", (RestoreStatus.to_query f)));
           Aws.Util.option_map v.owner
             (fun f -> Aws.Query.Pair ("Owner", (Owner.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair
                  ("StorageClass", (ObjectStorageClass.to_query f)));
           Aws.Util.option_map v.size
             (fun f -> Aws.Query.Pair ("Size", (Integer.to_query f)));
           Some
             (Aws.Query.Pair
                ("ChecksumAlgorithm.member",
                  (ChecksumAlgorithmList.to_query v.checksum_algorithm)));
           Aws.Util.option_map v.e_tag
             (fun f -> Aws.Query.Pair ("ETag", (String.to_query f)));
           Aws.Util.option_map v.last_modified
             (fun f -> Aws.Query.Pair ("LastModified", (DateTime.to_query f)));
           Aws.Util.option_map v.key
             (fun f -> Aws.Query.Pair ("Key", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.restore_status
              (fun f -> ("RestoreStatus", (RestoreStatus.to_json f)));
           Aws.Util.option_map v.owner
             (fun f -> ("Owner", (Owner.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f -> ("StorageClass", (ObjectStorageClass.to_json f)));
           Aws.Util.option_map v.size
             (fun f -> ("Size", (Integer.to_json f)));
           Some
             ("ChecksumAlgorithm",
               (ChecksumAlgorithmList.to_json v.checksum_algorithm));
           Aws.Util.option_map v.e_tag
             (fun f -> ("ETag", (String.to_json f)));
           Aws.Util.option_map v.last_modified
             (fun f -> ("LastModified", (DateTime.to_json f)));
           Aws.Util.option_map v.key (fun f -> ("Key", (String.to_json f)))])
    let of_json j =
      {
        key = (Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json);
        last_modified =
          (Aws.Util.option_map (Aws.Json.lookup j "LastModified")
             DateTime.of_json);
        e_tag =
          (Aws.Util.option_map (Aws.Json.lookup j "ETag") String.of_json);
        checksum_algorithm =
          (ChecksumAlgorithmList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "ChecksumAlgorithm")));
        size =
          (Aws.Util.option_map (Aws.Json.lookup j "Size") Integer.of_json);
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "StorageClass")
             ObjectStorageClass.of_json);
        owner =
          (Aws.Util.option_map (Aws.Json.lookup j "Owner") Owner.of_json);
        restore_status =
          (Aws.Util.option_map (Aws.Json.lookup j "RestoreStatus")
             RestoreStatus.of_json)
      }
  end
module ObjectList =
  struct
    type t = Object.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map Object.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list Object.to_query v
    let to_json v = `List (List.map Object.to_json v)
    let of_json j = Aws.Json.to_list Object.of_json j
  end
module ListObjectsOutput =
  struct
    type t =
      {
      is_truncated: Boolean.t option ;
      marker: String.t option ;
      next_marker: String.t option ;
      contents: ObjectList.t ;
      name: String.t option ;
      prefix: String.t option ;
      delimiter: String.t option ;
      max_keys: Integer.t option ;
      common_prefixes: CommonPrefixList.t ;
      encoding_type: EncodingType.t option ;
      request_charged: RequestCharged.t option }
    let make ?is_truncated  ?marker  ?next_marker  ?(contents= [])  ?name 
      ?prefix  ?delimiter  ?max_keys  ?(common_prefixes= [])  ?encoding_type 
      ?request_charged  () =
      {
        is_truncated;
        marker;
        next_marker;
        contents;
        name;
        prefix;
        delimiter;
        max_keys;
        common_prefixes;
        encoding_type;
        request_charged
      }
    let parse xml =
      Some
        {
          is_truncated =
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml)
               Boolean.parse);
          marker =
            (Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse);
          next_marker =
            (Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml)
               String.parse);
          contents = (Aws.Util.of_option [] (ObjectList.parse xml));
          name =
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse);
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          delimiter =
            (Aws.Util.option_bind (Aws.Xml.member "Delimiter" xml)
               String.parse);
          max_keys =
            (Aws.Util.option_bind (Aws.Xml.member "MaxKeys" xml)
               Integer.parse);
          common_prefixes =
            (Aws.Util.of_option [] (CommonPrefixList.parse xml));
          encoding_type =
            (Aws.Util.option_bind (Aws.Xml.member "EncodingType" xml)
               EncodingType.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.encoding_type
             (fun f ->
                Aws.Query.Pair ("EncodingType", (EncodingType.to_query f)));
           Some
             (Aws.Query.Pair
                ("CommonPrefixes.member",
                  (CommonPrefixList.to_query v.common_prefixes)));
           Aws.Util.option_map v.max_keys
             (fun f -> Aws.Query.Pair ("MaxKeys", (Integer.to_query f)));
           Aws.Util.option_map v.delimiter
             (fun f -> Aws.Query.Pair ("Delimiter", (String.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)));
           Aws.Util.option_map v.name
             (fun f -> Aws.Query.Pair ("Name", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("Contents.member", (ObjectList.to_query v.contents)));
           Aws.Util.option_map v.next_marker
             (fun f -> Aws.Query.Pair ("NextMarker", (String.to_query f)));
           Aws.Util.option_map v.marker
             (fun f -> Aws.Query.Pair ("Marker", (String.to_query f)));
           Aws.Util.option_map v.is_truncated
             (fun f -> Aws.Query.Pair ("IsTruncated", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.encoding_type
             (fun f -> ("EncodingType", (EncodingType.to_json f)));
           Some
             ("CommonPrefixes", (CommonPrefixList.to_json v.common_prefixes));
           Aws.Util.option_map v.max_keys
             (fun f -> ("MaxKeys", (Integer.to_json f)));
           Aws.Util.option_map v.delimiter
             (fun f -> ("Delimiter", (String.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)));
           Aws.Util.option_map v.name (fun f -> ("Name", (String.to_json f)));
           Some ("Contents", (ObjectList.to_json v.contents));
           Aws.Util.option_map v.next_marker
             (fun f -> ("NextMarker", (String.to_json f)));
           Aws.Util.option_map v.marker
             (fun f -> ("Marker", (String.to_json f)));
           Aws.Util.option_map v.is_truncated
             (fun f -> ("IsTruncated", (Boolean.to_json f)))])
    let of_json j =
      {
        is_truncated =
          (Aws.Util.option_map (Aws.Json.lookup j "IsTruncated")
             Boolean.of_json);
        marker =
          (Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json);
        next_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "NextMarker")
             String.of_json);
        contents =
          (ObjectList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Contents")));
        name =
          (Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json);
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        delimiter =
          (Aws.Util.option_map (Aws.Json.lookup j "Delimiter") String.of_json);
        max_keys =
          (Aws.Util.option_map (Aws.Json.lookup j "MaxKeys") Integer.of_json);
        common_prefixes =
          (CommonPrefixList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "CommonPrefixes")));
        encoding_type =
          (Aws.Util.option_map (Aws.Json.lookup j "EncodingType")
             EncodingType.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module DeleteBucketIntelligentTieringConfigurationRequest =
  struct
    type t = {
      bucket: String.t ;
      id: String.t }
    let make ~bucket  ~id  () = { bucket; id }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          id =
            (Aws.Xml.required "id"
               (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("id", (String.to_query v.id)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("id", (String.to_json v.id));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id")))
      }
  end
module ListBucketInventoryConfigurationsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      continuation_token: String.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?continuation_token  ?expected_bucket_owner  () =
      { bucket; continuation_token; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          continuation_token =
            (Aws.Util.option_bind (Aws.Xml.member "continuation-token" xml)
               String.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.continuation_token
             (fun f ->
                Aws.Query.Pair ("continuation-token", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.continuation_token
             (fun f -> ("continuation-token", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "continuation-token")
             String.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module ChecksumMode =
  struct
    type t =
      | ENABLED 
    let str_to_t = [("ENABLED", ENABLED)]
    let t_to_str = [(ENABLED, "ENABLED")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module HeadObjectRequest =
  struct
    type t =
      {
      bucket: String.t ;
      if_match: String.t option ;
      if_modified_since: DateTime.t option ;
      if_none_match: String.t option ;
      if_unmodified_since: DateTime.t option ;
      key: String.t ;
      range: String.t option ;
      version_id: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      request_payer: RequestPayer.t option ;
      part_number: Integer.t option ;
      expected_bucket_owner: String.t option ;
      checksum_mode: ChecksumMode.t option }
    let make ~bucket  ?if_match  ?if_modified_since  ?if_none_match 
      ?if_unmodified_since  ~key  ?range  ?version_id 
      ?s_s_e_customer_algorithm  ?s_s_e_customer_key 
      ?s_s_e_customer_key_m_d5  ?request_payer  ?part_number 
      ?expected_bucket_owner  ?checksum_mode  () =
      {
        bucket;
        if_match;
        if_modified_since;
        if_none_match;
        if_unmodified_since;
        key;
        range;
        version_id;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        request_payer;
        part_number;
        expected_bucket_owner;
        checksum_mode
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          if_match =
            (Aws.Util.option_bind (Aws.Xml.member "If-Match" xml)
               String.parse);
          if_modified_since =
            (Aws.Util.option_bind (Aws.Xml.member "If-Modified-Since" xml)
               DateTime.parse);
          if_none_match =
            (Aws.Util.option_bind (Aws.Xml.member "If-None-Match" xml)
               String.parse);
          if_unmodified_since =
            (Aws.Util.option_bind (Aws.Xml.member "If-Unmodified-Since" xml)
               DateTime.parse);
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          range =
            (Aws.Util.option_bind (Aws.Xml.member "Range" xml) String.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-customer-key"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          part_number =
            (Aws.Util.option_bind (Aws.Xml.member "partNumber" xml)
               Integer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          checksum_mode =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-mode" xml)
               ChecksumMode.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_mode
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-checksum-mode", (ChecksumMode.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.part_number
             (fun f -> Aws.Query.Pair ("partNumber", (Integer.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Aws.Util.option_map v.range
             (fun f -> Aws.Query.Pair ("Range", (String.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Aws.Util.option_map v.if_unmodified_since
             (fun f ->
                Aws.Query.Pair ("If-Unmodified-Since", (DateTime.to_query f)));
           Aws.Util.option_map v.if_none_match
             (fun f -> Aws.Query.Pair ("If-None-Match", (String.to_query f)));
           Aws.Util.option_map v.if_modified_since
             (fun f ->
                Aws.Query.Pair ("If-Modified-Since", (DateTime.to_query f)));
           Aws.Util.option_map v.if_match
             (fun f -> Aws.Query.Pair ("If-Match", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_mode
              (fun f -> ("x-amz-checksum-mode", (ChecksumMode.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.part_number
             (fun f -> ("partNumber", (Integer.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                ("x-amz-server-side-encryption-customer-key",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Aws.Util.option_map v.range
             (fun f -> ("Range", (String.to_json f)));
           Some ("Key", (String.to_json v.key));
           Aws.Util.option_map v.if_unmodified_since
             (fun f -> ("If-Unmodified-Since", (DateTime.to_json f)));
           Aws.Util.option_map v.if_none_match
             (fun f -> ("If-None-Match", (String.to_json f)));
           Aws.Util.option_map v.if_modified_since
             (fun f -> ("If-Modified-Since", (DateTime.to_json f)));
           Aws.Util.option_map v.if_match
             (fun f -> ("If-Match", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        if_match =
          (Aws.Util.option_map (Aws.Json.lookup j "If-Match") String.of_json);
        if_modified_since =
          (Aws.Util.option_map (Aws.Json.lookup j "If-Modified-Since")
             DateTime.of_json);
        if_none_match =
          (Aws.Util.option_map (Aws.Json.lookup j "If-None-Match")
             String.of_json);
        if_unmodified_since =
          (Aws.Util.option_map (Aws.Json.lookup j "If-Unmodified-Since")
             DateTime.of_json);
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        range =
          (Aws.Util.option_map (Aws.Json.lookup j "Range") String.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-customer-key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        part_number =
          (Aws.Util.option_map (Aws.Json.lookup j "partNumber")
             Integer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        checksum_mode =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-mode")
             ChecksumMode.of_json)
      }
  end
module GetBucketWebsiteOutput =
  struct
    type t =
      {
      redirect_all_requests_to: RedirectAllRequestsTo.t option ;
      index_document: IndexDocument.t option ;
      error_document: ErrorDocument.t option ;
      routing_rules: RoutingRules.t }
    let make ?redirect_all_requests_to  ?index_document  ?error_document 
      ?(routing_rules= [])  () =
      {
        redirect_all_requests_to;
        index_document;
        error_document;
        routing_rules
      }
    let parse xml =
      Some
        {
          redirect_all_requests_to =
            (Aws.Util.option_bind
               (Aws.Xml.member "RedirectAllRequestsTo" xml)
               RedirectAllRequestsTo.parse);
          index_document =
            (Aws.Util.option_bind (Aws.Xml.member "IndexDocument" xml)
               IndexDocument.parse);
          error_document =
            (Aws.Util.option_bind (Aws.Xml.member "ErrorDocument" xml)
               ErrorDocument.parse);
          routing_rules =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "RoutingRules" xml)
                  RoutingRules.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("RoutingRules.member",
                   (RoutingRules.to_query v.routing_rules)));
           Aws.Util.option_map v.error_document
             (fun f ->
                Aws.Query.Pair ("ErrorDocument", (ErrorDocument.to_query f)));
           Aws.Util.option_map v.index_document
             (fun f ->
                Aws.Query.Pair ("IndexDocument", (IndexDocument.to_query f)));
           Aws.Util.option_map v.redirect_all_requests_to
             (fun f ->
                Aws.Query.Pair
                  ("RedirectAllRequestsTo",
                    (RedirectAllRequestsTo.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("RoutingRules", (RoutingRules.to_json v.routing_rules));
           Aws.Util.option_map v.error_document
             (fun f -> ("ErrorDocument", (ErrorDocument.to_json f)));
           Aws.Util.option_map v.index_document
             (fun f -> ("IndexDocument", (IndexDocument.to_json f)));
           Aws.Util.option_map v.redirect_all_requests_to
             (fun f ->
                ("RedirectAllRequestsTo", (RedirectAllRequestsTo.to_json f)))])
    let of_json j =
      {
        redirect_all_requests_to =
          (Aws.Util.option_map (Aws.Json.lookup j "RedirectAllRequestsTo")
             RedirectAllRequestsTo.of_json);
        index_document =
          (Aws.Util.option_map (Aws.Json.lookup j "IndexDocument")
             IndexDocument.of_json);
        error_document =
          (Aws.Util.option_map (Aws.Json.lookup j "ErrorDocument")
             ErrorDocument.of_json);
        routing_rules =
          (RoutingRules.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "RoutingRules")))
      }
  end
module AbortMultipartUploadOutput =
  struct
    type t = {
      request_charged: RequestCharged.t option }
    let make ?request_charged  () = { request_charged }
    let parse xml =
      Some
        {
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)))])
    let of_json j =
      {
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module SseKmsEncryptedObjectsStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)]
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module SseKmsEncryptedObjects =
  struct
    type t = {
      status: SseKmsEncryptedObjectsStatus.t }
    let make ~status  () = { status }
    let parse xml =
      Some
        {
          status =
            (Aws.Xml.required "Status"
               (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
                  SseKmsEncryptedObjectsStatus.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("Status", (SseKmsEncryptedObjectsStatus.to_query v.status)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Status", (SseKmsEncryptedObjectsStatus.to_json v.status))])
    let of_json j =
      {
        status =
          (SseKmsEncryptedObjectsStatus.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Status")))
      }
  end
module ReplicaModifications =
  struct
    type t = {
      status: ReplicaModificationsStatus.t }
    let make ~status  () = { status }
    let parse xml =
      Some
        {
          status =
            (Aws.Xml.required "Status"
               (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
                  ReplicaModificationsStatus.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("Status", (ReplicaModificationsStatus.to_query v.status)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Status", (ReplicaModificationsStatus.to_json v.status))])
    let of_json j =
      {
        status =
          (ReplicaModificationsStatus.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Status")))
      }
  end
module SourceSelectionCriteria =
  struct
    type t =
      {
      sse_kms_encrypted_objects: SseKmsEncryptedObjects.t option ;
      replica_modifications: ReplicaModifications.t option }
    let make ?sse_kms_encrypted_objects  ?replica_modifications  () =
      { sse_kms_encrypted_objects; replica_modifications }
    let parse xml =
      Some
        {
          sse_kms_encrypted_objects =
            (Aws.Util.option_bind
               (Aws.Xml.member "SseKmsEncryptedObjects" xml)
               SseKmsEncryptedObjects.parse);
          replica_modifications =
            (Aws.Util.option_bind (Aws.Xml.member "ReplicaModifications" xml)
               ReplicaModifications.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.replica_modifications
              (fun f ->
                 Aws.Query.Pair
                   ("ReplicaModifications",
                     (ReplicaModifications.to_query f)));
           Aws.Util.option_map v.sse_kms_encrypted_objects
             (fun f ->
                Aws.Query.Pair
                  ("SseKmsEncryptedObjects",
                    (SseKmsEncryptedObjects.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.replica_modifications
              (fun f ->
                 ("ReplicaModifications", (ReplicaModifications.to_json f)));
           Aws.Util.option_map v.sse_kms_encrypted_objects
             (fun f ->
                ("SseKmsEncryptedObjects",
                  (SseKmsEncryptedObjects.to_json f)))])
    let of_json j =
      {
        sse_kms_encrypted_objects =
          (Aws.Util.option_map (Aws.Json.lookup j "SseKmsEncryptedObjects")
             SseKmsEncryptedObjects.of_json);
        replica_modifications =
          (Aws.Util.option_map (Aws.Json.lookup j "ReplicaModifications")
             ReplicaModifications.of_json)
      }
  end
module ReplicationRuleStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)]
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module Metrics =
  struct
    type t =
      {
      status: MetricsStatus.t ;
      event_threshold: ReplicationTimeValue.t option }
    let make ~status  ?event_threshold  () = { status; event_threshold }
    let parse xml =
      Some
        {
          status =
            (Aws.Xml.required "Status"
               (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
                  MetricsStatus.parse));
          event_threshold =
            (Aws.Util.option_bind (Aws.Xml.member "EventThreshold" xml)
               ReplicationTimeValue.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.event_threshold
              (fun f ->
                 Aws.Query.Pair
                   ("EventThreshold", (ReplicationTimeValue.to_query f)));
           Some
             (Aws.Query.Pair ("Status", (MetricsStatus.to_query v.status)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.event_threshold
              (fun f -> ("EventThreshold", (ReplicationTimeValue.to_json f)));
           Some ("Status", (MetricsStatus.to_json v.status))])
    let of_json j =
      {
        status =
          (MetricsStatus.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Status")));
        event_threshold =
          (Aws.Util.option_map (Aws.Json.lookup j "EventThreshold")
             ReplicationTimeValue.of_json)
      }
  end
module Destination =
  struct
    type t =
      {
      bucket: String.t ;
      account: String.t option ;
      storage_class: StorageClass.t option ;
      access_control_translation: AccessControlTranslation.t option ;
      encryption_configuration: EncryptionConfiguration.t option ;
      replication_time: ReplicationTime.t option ;
      metrics: Metrics.t option }
    let make ~bucket  ?account  ?storage_class  ?access_control_translation 
      ?encryption_configuration  ?replication_time  ?metrics  () =
      {
        bucket;
        account;
        storage_class;
        access_control_translation;
        encryption_configuration;
        replication_time;
        metrics
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          account =
            (Aws.Util.option_bind (Aws.Xml.member "Account" xml) String.parse);
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "StorageClass" xml)
               StorageClass.parse);
          access_control_translation =
            (Aws.Util.option_bind
               (Aws.Xml.member "AccessControlTranslation" xml)
               AccessControlTranslation.parse);
          encryption_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "EncryptionConfiguration" xml)
               EncryptionConfiguration.parse);
          replication_time =
            (Aws.Util.option_bind (Aws.Xml.member "ReplicationTime" xml)
               ReplicationTime.parse);
          metrics =
            (Aws.Util.option_bind (Aws.Xml.member "Metrics" xml)
               Metrics.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.metrics
              (fun f -> Aws.Query.Pair ("Metrics", (Metrics.to_query f)));
           Aws.Util.option_map v.replication_time
             (fun f ->
                Aws.Query.Pair
                  ("ReplicationTime", (ReplicationTime.to_query f)));
           Aws.Util.option_map v.encryption_configuration
             (fun f ->
                Aws.Query.Pair
                  ("EncryptionConfiguration",
                    (EncryptionConfiguration.to_query f)));
           Aws.Util.option_map v.access_control_translation
             (fun f ->
                Aws.Query.Pair
                  ("AccessControlTranslation",
                    (AccessControlTranslation.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair ("StorageClass", (StorageClass.to_query f)));
           Aws.Util.option_map v.account
             (fun f -> Aws.Query.Pair ("Account", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.metrics
              (fun f -> ("Metrics", (Metrics.to_json f)));
           Aws.Util.option_map v.replication_time
             (fun f -> ("ReplicationTime", (ReplicationTime.to_json f)));
           Aws.Util.option_map v.encryption_configuration
             (fun f ->
                ("EncryptionConfiguration",
                  (EncryptionConfiguration.to_json f)));
           Aws.Util.option_map v.access_control_translation
             (fun f ->
                ("AccessControlTranslation",
                  (AccessControlTranslation.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f -> ("StorageClass", (StorageClass.to_json f)));
           Aws.Util.option_map v.account
             (fun f -> ("Account", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        account =
          (Aws.Util.option_map (Aws.Json.lookup j "Account") String.of_json);
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "StorageClass")
             StorageClass.of_json);
        access_control_translation =
          (Aws.Util.option_map (Aws.Json.lookup j "AccessControlTranslation")
             AccessControlTranslation.of_json);
        encryption_configuration =
          (Aws.Util.option_map (Aws.Json.lookup j "EncryptionConfiguration")
             EncryptionConfiguration.of_json);
        replication_time =
          (Aws.Util.option_map (Aws.Json.lookup j "ReplicationTime")
             ReplicationTime.of_json);
        metrics =
          (Aws.Util.option_map (Aws.Json.lookup j "Metrics") Metrics.of_json)
      }
  end
module DeleteMarkerReplication =
  struct
    type t = {
      status: DeleteMarkerReplicationStatus.t option }
    let make ?status  () = { status }
    let parse xml =
      Some
        {
          status =
            (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
               DeleteMarkerReplicationStatus.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.status
              (fun f ->
                 Aws.Query.Pair
                   ("Status", (DeleteMarkerReplicationStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.status
              (fun f -> ("Status", (DeleteMarkerReplicationStatus.to_json f)))])
    let of_json j =
      {
        status =
          (Aws.Util.option_map (Aws.Json.lookup j "Status")
             DeleteMarkerReplicationStatus.of_json)
      }
  end
module ReplicationRule =
  struct
    type t =
      {
      i_d: String.t option ;
      priority: Integer.t option ;
      prefix: String.t option ;
      filter: ReplicationRuleFilter.t option ;
      status: ReplicationRuleStatus.t ;
      source_selection_criteria: SourceSelectionCriteria.t option ;
      existing_object_replication: ExistingObjectReplication.t option ;
      destination: Destination.t ;
      delete_marker_replication: DeleteMarkerReplication.t option }
    let make ?i_d  ?priority  ?prefix  ?filter  ~status 
      ?source_selection_criteria  ?existing_object_replication  ~destination 
      ?delete_marker_replication  () =
      {
        i_d;
        priority;
        prefix;
        filter;
        status;
        source_selection_criteria;
        existing_object_replication;
        destination;
        delete_marker_replication
      }
    let parse xml =
      Some
        {
          i_d = (Aws.Util.option_bind (Aws.Xml.member "ID" xml) String.parse);
          priority =
            (Aws.Util.option_bind (Aws.Xml.member "Priority" xml)
               Integer.parse);
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          filter =
            (Aws.Util.option_bind (Aws.Xml.member "Filter" xml)
               ReplicationRuleFilter.parse);
          status =
            (Aws.Xml.required "Status"
               (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
                  ReplicationRuleStatus.parse));
          source_selection_criteria =
            (Aws.Util.option_bind
               (Aws.Xml.member "SourceSelectionCriteria" xml)
               SourceSelectionCriteria.parse);
          existing_object_replication =
            (Aws.Util.option_bind
               (Aws.Xml.member "ExistingObjectReplication" xml)
               ExistingObjectReplication.parse);
          destination =
            (Aws.Xml.required "Destination"
               (Aws.Util.option_bind (Aws.Xml.member "Destination" xml)
                  Destination.parse));
          delete_marker_replication =
            (Aws.Util.option_bind
               (Aws.Xml.member "DeleteMarkerReplication" xml)
               DeleteMarkerReplication.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.delete_marker_replication
              (fun f ->
                 Aws.Query.Pair
                   ("DeleteMarkerReplication",
                     (DeleteMarkerReplication.to_query f)));
           Some
             (Aws.Query.Pair
                ("Destination", (Destination.to_query v.destination)));
           Aws.Util.option_map v.existing_object_replication
             (fun f ->
                Aws.Query.Pair
                  ("ExistingObjectReplication",
                    (ExistingObjectReplication.to_query f)));
           Aws.Util.option_map v.source_selection_criteria
             (fun f ->
                Aws.Query.Pair
                  ("SourceSelectionCriteria",
                    (SourceSelectionCriteria.to_query f)));
           Some
             (Aws.Query.Pair
                ("Status", (ReplicationRuleStatus.to_query v.status)));
           Aws.Util.option_map v.filter
             (fun f ->
                Aws.Query.Pair ("Filter", (ReplicationRuleFilter.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)));
           Aws.Util.option_map v.priority
             (fun f -> Aws.Query.Pair ("Priority", (Integer.to_query f)));
           Aws.Util.option_map v.i_d
             (fun f -> Aws.Query.Pair ("ID", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.delete_marker_replication
              (fun f ->
                 ("DeleteMarkerReplication",
                   (DeleteMarkerReplication.to_json f)));
           Some ("Destination", (Destination.to_json v.destination));
           Aws.Util.option_map v.existing_object_replication
             (fun f ->
                ("ExistingObjectReplication",
                  (ExistingObjectReplication.to_json f)));
           Aws.Util.option_map v.source_selection_criteria
             (fun f ->
                ("SourceSelectionCriteria",
                  (SourceSelectionCriteria.to_json f)));
           Some ("Status", (ReplicationRuleStatus.to_json v.status));
           Aws.Util.option_map v.filter
             (fun f -> ("Filter", (ReplicationRuleFilter.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)));
           Aws.Util.option_map v.priority
             (fun f -> ("Priority", (Integer.to_json f)));
           Aws.Util.option_map v.i_d (fun f -> ("ID", (String.to_json f)))])
    let of_json j =
      {
        i_d = (Aws.Util.option_map (Aws.Json.lookup j "ID") String.of_json);
        priority =
          (Aws.Util.option_map (Aws.Json.lookup j "Priority") Integer.of_json);
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        filter =
          (Aws.Util.option_map (Aws.Json.lookup j "Filter")
             ReplicationRuleFilter.of_json);
        status =
          (ReplicationRuleStatus.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Status")));
        source_selection_criteria =
          (Aws.Util.option_map (Aws.Json.lookup j "SourceSelectionCriteria")
             SourceSelectionCriteria.of_json);
        existing_object_replication =
          (Aws.Util.option_map
             (Aws.Json.lookup j "ExistingObjectReplication")
             ExistingObjectReplication.of_json);
        destination =
          (Destination.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Destination")));
        delete_marker_replication =
          (Aws.Util.option_map (Aws.Json.lookup j "DeleteMarkerReplication")
             DeleteMarkerReplication.of_json)
      }
  end
module ReplicationRules =
  struct
    type t = ReplicationRule.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map ReplicationRule.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list ReplicationRule.to_query v
    let to_json v = `List (List.map ReplicationRule.to_json v)
    let of_json j = Aws.Json.to_list ReplicationRule.of_json j
  end
module ReplicationConfiguration =
  struct
    type t = {
      role: String.t ;
      rules: ReplicationRules.t }
    let make ~role  ~rules  () = { role; rules }
    let parse xml =
      Some
        {
          role =
            (Aws.Xml.required "Role"
               (Aws.Util.option_bind (Aws.Xml.member "Role" xml) String.parse));
          rules = (Aws.Xml.required "Rule" (ReplicationRules.parse xml))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair ("Rule", (ReplicationRules.to_query v.rules)));
           Some (Aws.Query.Pair ("Role", (String.to_query v.role)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Rule", (ReplicationRules.to_json v.rules));
           Some ("Role", (String.to_json v.role))])
    let of_json j =
      {
        role =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Role")));
        rules =
          (ReplicationRules.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Rule")))
      }
  end
module GetBucketReplicationOutput =
  struct
    type t = {
      replication_configuration: ReplicationConfiguration.t option }
    let make ?replication_configuration  () = { replication_configuration }
    let parse xml =
      Some
        {
          replication_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicationConfiguration" xml)
               ReplicationConfiguration.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.replication_configuration
              (fun f ->
                 Aws.Query.Pair
                   ("ReplicationConfiguration",
                     (ReplicationConfiguration.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.replication_configuration
              (fun f ->
                 ("ReplicationConfiguration",
                   (ReplicationConfiguration.to_json f)))])
    let of_json j =
      {
        replication_configuration =
          (Aws.Util.option_map (Aws.Json.lookup j "ReplicationConfiguration")
             ReplicationConfiguration.of_json)
      }
  end
module GetBucketLoggingRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module DeleteBucketEncryptionRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetObjectTorrentRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      request_payer: RequestPayer.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~key  ?request_payer  ?expected_bucket_owner  () =
      { bucket; key; request_payer; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module BucketAlreadyOwnedByYou =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])
    let to_json v = `Assoc (Aws.Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteBucketTaggingRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module DeletePublicAccessBlockRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module PutObjectLegalHoldOutput =
  struct
    type t = {
      request_charged: RequestCharged.t option }
    let make ?request_charged  () = { request_charged }
    let parse xml =
      Some
        {
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)))])
    let of_json j =
      {
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module GetBucketRequestPaymentOutput =
  struct
    type t = {
      payer: Payer.t option }
    let make ?payer  () = { payer }
    let parse xml =
      Some
        {
          payer =
            (Aws.Util.option_bind (Aws.Xml.member "Payer" xml) Payer.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.payer
              (fun f -> Aws.Query.Pair ("Payer", (Payer.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.payer
              (fun f -> ("Payer", (Payer.to_json f)))])
    let of_json j =
      {
        payer =
          (Aws.Util.option_map (Aws.Json.lookup j "Payer") Payer.of_json)
      }
  end
module PutBucketCorsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      c_o_r_s_configuration: CORSConfiguration.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~c_o_r_s_configuration  ?content_m_d5 
      ?checksum_algorithm  ?expected_bucket_owner  () =
      {
        bucket;
        c_o_r_s_configuration;
        content_m_d5;
        checksum_algorithm;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          c_o_r_s_configuration =
            (Aws.Xml.required "CORSConfiguration"
               (Aws.Util.option_bind (Aws.Xml.member "CORSConfiguration" xml)
                  CORSConfiguration.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("CORSConfiguration",
                  (CORSConfiguration.to_query v.c_o_r_s_configuration)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some
             ("CORSConfiguration",
               (CORSConfiguration.to_json v.c_o_r_s_configuration));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        c_o_r_s_configuration =
          (CORSConfiguration.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "CORSConfiguration")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module ListBucketAnalyticsConfigurationsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      continuation_token: String.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?continuation_token  ?expected_bucket_owner  () =
      { bucket; continuation_token; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          continuation_token =
            (Aws.Util.option_bind (Aws.Xml.member "continuation-token" xml)
               String.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.continuation_token
             (fun f ->
                Aws.Query.Pair ("continuation-token", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.continuation_token
             (fun f -> ("continuation-token", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "continuation-token")
             String.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module MFADeleteStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)]
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module CreateBucketOutput =
  struct
    type t = {
      location: String.t option }
    let make ?location  () = { location }
    let parse xml =
      Some
        {
          location =
            (Aws.Util.option_bind (Aws.Xml.member "Location" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.location
              (fun f -> Aws.Query.Pair ("Location", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.location
              (fun f -> ("Location", (String.to_json f)))])
    let of_json j =
      {
        location =
          (Aws.Util.option_map (Aws.Json.lookup j "Location") String.of_json)
      }
  end
module GetBucketCorsRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetBucketPolicyStatusRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetPublicAccessBlockOutput =
  struct
    type t =
      {
      public_access_block_configuration:
        PublicAccessBlockConfiguration.t option }
    let make ?public_access_block_configuration  () =
      { public_access_block_configuration }
    let parse xml =
      Some
        {
          public_access_block_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "PublicAccessBlockConfiguration" xml)
               PublicAccessBlockConfiguration.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.public_access_block_configuration
              (fun f ->
                 Aws.Query.Pair
                   ("PublicAccessBlockConfiguration",
                     (PublicAccessBlockConfiguration.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.public_access_block_configuration
              (fun f ->
                 ("PublicAccessBlockConfiguration",
                   (PublicAccessBlockConfiguration.to_json f)))])
    let of_json j =
      {
        public_access_block_configuration =
          (Aws.Util.option_map
             (Aws.Json.lookup j "PublicAccessBlockConfiguration")
             PublicAccessBlockConfiguration.of_json)
      }
  end
module GetBucketWebsiteRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetBucketLifecycleRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module PutBucketWebsiteRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      website_configuration: WebsiteConfiguration.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?content_m_d5  ?checksum_algorithm 
      ~website_configuration  ?expected_bucket_owner  () =
      {
        bucket;
        content_m_d5;
        checksum_algorithm;
        website_configuration;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          website_configuration =
            (Aws.Xml.required "WebsiteConfiguration"
               (Aws.Util.option_bind
                  (Aws.Xml.member "WebsiteConfiguration" xml)
                  WebsiteConfiguration.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("WebsiteConfiguration",
                  (WebsiteConfiguration.to_query v.website_configuration)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some
             ("WebsiteConfiguration",
               (WebsiteConfiguration.to_json v.website_configuration));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        website_configuration =
          (WebsiteConfiguration.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "WebsiteConfiguration")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module DeleteBucketWebsiteRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetObjectLockConfigurationOutput =
  struct
    type t = {
      object_lock_configuration: ObjectLockConfiguration.t option }
    let make ?object_lock_configuration  () = { object_lock_configuration }
    let parse xml =
      Some
        {
          object_lock_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "ObjectLockConfiguration" xml)
               ObjectLockConfiguration.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.object_lock_configuration
              (fun f ->
                 Aws.Query.Pair
                   ("ObjectLockConfiguration",
                     (ObjectLockConfiguration.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.object_lock_configuration
              (fun f ->
                 ("ObjectLockConfiguration",
                   (ObjectLockConfiguration.to_json f)))])
    let of_json j =
      {
        object_lock_configuration =
          (Aws.Util.option_map (Aws.Json.lookup j "ObjectLockConfiguration")
             ObjectLockConfiguration.of_json)
      }
  end
module MetadataDirective =
  struct
    type t =
      | COPY 
      | REPLACE 
    let str_to_t = [("REPLACE", REPLACE); ("COPY", COPY)]
    let t_to_str = [(REPLACE, "REPLACE"); (COPY, "COPY")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module CopyObjectRequest =
  struct
    type t =
      {
      a_c_l: ObjectCannedACL.t option ;
      bucket: String.t ;
      cache_control: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      content_disposition: String.t option ;
      content_encoding: String.t option ;
      content_language: String.t option ;
      content_type: String.t option ;
      copy_source: String.t ;
      copy_source_if_match: String.t option ;
      copy_source_if_modified_since: DateTime.t option ;
      copy_source_if_none_match: String.t option ;
      copy_source_if_unmodified_since: DateTime.t option ;
      expires: DateTime.t option ;
      grant_full_control: String.t option ;
      grant_read: String.t option ;
      grant_read_a_c_p: String.t option ;
      grant_write_a_c_p: String.t option ;
      key: String.t ;
      metadata: Metadata.t option ;
      metadata_directive: MetadataDirective.t option ;
      tagging_directive: TaggingDirective.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      storage_class: StorageClass.t option ;
      website_redirect_location: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      s_s_e_k_m_s_encryption_context: String.t option ;
      bucket_key_enabled: Boolean.t option ;
      copy_source_s_s_e_customer_algorithm: String.t option ;
      copy_source_s_s_e_customer_key: String.t option ;
      copy_source_s_s_e_customer_key_m_d5: String.t option ;
      request_payer: RequestPayer.t option ;
      tagging: String.t option ;
      object_lock_mode: ObjectLockMode.t option ;
      object_lock_retain_until_date: DateTime.t option ;
      object_lock_legal_hold_status: ObjectLockLegalHoldStatus.t option ;
      expected_bucket_owner: String.t option ;
      expected_source_bucket_owner: String.t option }
    let make ?a_c_l  ~bucket  ?cache_control  ?checksum_algorithm 
      ?content_disposition  ?content_encoding  ?content_language 
      ?content_type  ~copy_source  ?copy_source_if_match 
      ?copy_source_if_modified_since  ?copy_source_if_none_match 
      ?copy_source_if_unmodified_since  ?expires  ?grant_full_control 
      ?grant_read  ?grant_read_a_c_p  ?grant_write_a_c_p  ~key  ?metadata 
      ?metadata_directive  ?tagging_directive  ?server_side_encryption 
      ?storage_class  ?website_redirect_location  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key  ?s_s_e_customer_key_m_d5  ?s_s_e_k_m_s_key_id 
      ?s_s_e_k_m_s_encryption_context  ?bucket_key_enabled 
      ?copy_source_s_s_e_customer_algorithm  ?copy_source_s_s_e_customer_key 
      ?copy_source_s_s_e_customer_key_m_d5  ?request_payer  ?tagging 
      ?object_lock_mode  ?object_lock_retain_until_date 
      ?object_lock_legal_hold_status  ?expected_bucket_owner 
      ?expected_source_bucket_owner  () =
      {
        a_c_l;
        bucket;
        cache_control;
        checksum_algorithm;
        content_disposition;
        content_encoding;
        content_language;
        content_type;
        copy_source;
        copy_source_if_match;
        copy_source_if_modified_since;
        copy_source_if_none_match;
        copy_source_if_unmodified_since;
        expires;
        grant_full_control;
        grant_read;
        grant_read_a_c_p;
        grant_write_a_c_p;
        key;
        metadata;
        metadata_directive;
        tagging_directive;
        server_side_encryption;
        storage_class;
        website_redirect_location;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        s_s_e_k_m_s_encryption_context;
        bucket_key_enabled;
        copy_source_s_s_e_customer_algorithm;
        copy_source_s_s_e_customer_key;
        copy_source_s_s_e_customer_key_m_d5;
        request_payer;
        tagging;
        object_lock_mode;
        object_lock_retain_until_date;
        object_lock_legal_hold_status;
        expected_bucket_owner;
        expected_source_bucket_owner
      }
    let parse xml =
      Some
        {
          a_c_l =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-acl" xml)
               ObjectCannedACL.parse);
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          cache_control =
            (Aws.Util.option_bind (Aws.Xml.member "Cache-Control" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          content_disposition =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Disposition" xml)
               String.parse);
          content_encoding =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Encoding" xml)
               String.parse);
          content_language =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Language" xml)
               String.parse);
          content_type =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Type" xml)
               String.parse);
          copy_source =
            (Aws.Xml.required "x-amz-copy-source"
               (Aws.Util.option_bind (Aws.Xml.member "x-amz-copy-source" xml)
                  String.parse));
          copy_source_if_match =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-copy-source-if-match" xml) String.parse);
          copy_source_if_modified_since =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-copy-source-if-modified-since" xml)
               DateTime.parse);
          copy_source_if_none_match =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-copy-source-if-none-match" xml)
               String.parse);
          copy_source_if_unmodified_since =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-copy-source-if-unmodified-since" xml)
               DateTime.parse);
          expires =
            (Aws.Util.option_bind (Aws.Xml.member "Expires" xml)
               DateTime.parse);
          grant_full_control =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-grant-full-control" xml) String.parse);
          grant_read =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-read" xml)
               String.parse);
          grant_read_a_c_p =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-grant-read-acp" xml)
               String.parse);
          grant_write_a_c_p =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-grant-write-acp" xml) String.parse);
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          metadata =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-meta-" xml)
               Metadata.parse);
          metadata_directive =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-metadata-directive" xml)
               MetadataDirective.parse);
          tagging_directive =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-tagging-directive" xml)
               TaggingDirective.parse);
          server_side_encryption =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-storage-class" xml)
               StorageClass.parse);
          website_redirect_location =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-website-redirect-location" xml)
               String.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-customer-key"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          s_s_e_k_m_s_key_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-aws-kms-key-id"
                  xml) String.parse);
          s_s_e_k_m_s_encryption_context =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-context" xml)
               String.parse);
          bucket_key_enabled =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-bucket-key-enabled" xml)
               Boolean.parse);
          copy_source_s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-copy-source-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          copy_source_s_s_e_customer_key =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-copy-source-server-side-encryption-customer-key" xml)
               String.parse);
          copy_source_s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-copy-source-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          tagging =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-tagging" xml)
               String.parse);
          object_lock_mode =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-mode" xml)
               ObjectLockMode.parse);
          object_lock_retain_until_date =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-retain-until-date" xml)
               DateTime.parse);
          object_lock_legal_hold_status =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-object-lock-legal-hold" xml)
               ObjectLockLegalHoldStatus.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          expected_source_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-source-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_source_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-source-expected-bucket-owner",
                     (String.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.object_lock_legal_hold_status
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-object-lock-legal-hold",
                    (ObjectLockLegalHoldStatus.to_query f)));
           Aws.Util.option_map v.object_lock_retain_until_date
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-object-lock-retain-until-date",
                    (DateTime.to_query f)));
           Aws.Util.option_map v.object_lock_mode
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-object-lock-mode", (ObjectLockMode.to_query f)));
           Aws.Util.option_map v.tagging
             (fun f -> Aws.Query.Pair ("x-amz-tagging", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.copy_source_s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.copy_source_s_s_e_customer_key
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-server-side-encryption-customer-key",
                    (String.to_query f)));
           Aws.Util.option_map v.copy_source_s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-bucket-key-enabled",
                    (Boolean.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_encryption_context
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-context",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.website_redirect_location
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-website-redirect-location", (String.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-storage-class", (StorageClass.to_query f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Aws.Util.option_map v.tagging_directive
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-tagging-directive", (TaggingDirective.to_query f)));
           Aws.Util.option_map v.metadata_directive
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-metadata-directive",
                    (MetadataDirective.to_query f)));
           Aws.Util.option_map v.metadata
             (fun f -> Aws.Query.Pair ("x-amz-meta-", (Metadata.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Aws.Util.option_map v.grant_write_a_c_p
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-write-acp", (String.to_query f)));
           Aws.Util.option_map v.grant_read_a_c_p
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-read-acp", (String.to_query f)));
           Aws.Util.option_map v.grant_read
             (fun f ->
                Aws.Query.Pair ("x-amz-grant-read", (String.to_query f)));
           Aws.Util.option_map v.grant_full_control
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-grant-full-control", (String.to_query f)));
           Aws.Util.option_map v.expires
             (fun f -> Aws.Query.Pair ("Expires", (DateTime.to_query f)));
           Aws.Util.option_map v.copy_source_if_unmodified_since
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-if-unmodified-since",
                    (DateTime.to_query f)));
           Aws.Util.option_map v.copy_source_if_none_match
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-if-none-match", (String.to_query f)));
           Aws.Util.option_map v.copy_source_if_modified_since
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-if-modified-since",
                    (DateTime.to_query f)));
           Aws.Util.option_map v.copy_source_if_match
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-if-match", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("x-amz-copy-source", (String.to_query v.copy_source)));
           Aws.Util.option_map v.content_type
             (fun f -> Aws.Query.Pair ("Content-Type", (String.to_query f)));
           Aws.Util.option_map v.content_language
             (fun f ->
                Aws.Query.Pair ("Content-Language", (String.to_query f)));
           Aws.Util.option_map v.content_encoding
             (fun f ->
                Aws.Query.Pair ("Content-Encoding", (String.to_query f)));
           Aws.Util.option_map v.content_disposition
             (fun f ->
                Aws.Query.Pair ("Content-Disposition", (String.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.cache_control
             (fun f -> Aws.Query.Pair ("Cache-Control", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)));
           Aws.Util.option_map v.a_c_l
             (fun f ->
                Aws.Query.Pair ("x-amz-acl", (ObjectCannedACL.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_source_bucket_owner
              (fun f ->
                 ("x-amz-source-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.object_lock_legal_hold_status
             (fun f ->
                ("x-amz-object-lock-legal-hold",
                  (ObjectLockLegalHoldStatus.to_json f)));
           Aws.Util.option_map v.object_lock_retain_until_date
             (fun f ->
                ("x-amz-object-lock-retain-until-date", (DateTime.to_json f)));
           Aws.Util.option_map v.object_lock_mode
             (fun f -> ("x-amz-object-lock-mode", (ObjectLockMode.to_json f)));
           Aws.Util.option_map v.tagging
             (fun f -> ("x-amz-tagging", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.copy_source_s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-copy-source-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.copy_source_s_s_e_customer_key
             (fun f ->
                ("x-amz-copy-source-server-side-encryption-customer-key",
                  (String.to_json f)));
           Aws.Util.option_map v.copy_source_s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-copy-source-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                ("x-amz-server-side-encryption-bucket-key-enabled",
                  (Boolean.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_encryption_context
             (fun f ->
                ("x-amz-server-side-encryption-context", (String.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                ("x-amz-server-side-encryption-aws-kms-key-id",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                ("x-amz-server-side-encryption-customer-key",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.website_redirect_location
             (fun f ->
                ("x-amz-website-redirect-location", (String.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f -> ("x-amz-storage-class", (StorageClass.to_json f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                ("x-amz-server-side-encryption",
                  (ServerSideEncryption.to_json f)));
           Aws.Util.option_map v.tagging_directive
             (fun f ->
                ("x-amz-tagging-directive", (TaggingDirective.to_json f)));
           Aws.Util.option_map v.metadata_directive
             (fun f ->
                ("x-amz-metadata-directive", (MetadataDirective.to_json f)));
           Aws.Util.option_map v.metadata
             (fun f -> ("x-amz-meta-", (Metadata.to_json f)));
           Some ("Key", (String.to_json v.key));
           Aws.Util.option_map v.grant_write_a_c_p
             (fun f -> ("x-amz-grant-write-acp", (String.to_json f)));
           Aws.Util.option_map v.grant_read_a_c_p
             (fun f -> ("x-amz-grant-read-acp", (String.to_json f)));
           Aws.Util.option_map v.grant_read
             (fun f -> ("x-amz-grant-read", (String.to_json f)));
           Aws.Util.option_map v.grant_full_control
             (fun f -> ("x-amz-grant-full-control", (String.to_json f)));
           Aws.Util.option_map v.expires
             (fun f -> ("Expires", (DateTime.to_json f)));
           Aws.Util.option_map v.copy_source_if_unmodified_since
             (fun f ->
                ("x-amz-copy-source-if-unmodified-since",
                  (DateTime.to_json f)));
           Aws.Util.option_map v.copy_source_if_none_match
             (fun f ->
                ("x-amz-copy-source-if-none-match", (String.to_json f)));
           Aws.Util.option_map v.copy_source_if_modified_since
             (fun f ->
                ("x-amz-copy-source-if-modified-since", (DateTime.to_json f)));
           Aws.Util.option_map v.copy_source_if_match
             (fun f -> ("x-amz-copy-source-if-match", (String.to_json f)));
           Some ("x-amz-copy-source", (String.to_json v.copy_source));
           Aws.Util.option_map v.content_type
             (fun f -> ("Content-Type", (String.to_json f)));
           Aws.Util.option_map v.content_language
             (fun f -> ("Content-Language", (String.to_json f)));
           Aws.Util.option_map v.content_encoding
             (fun f -> ("Content-Encoding", (String.to_json f)));
           Aws.Util.option_map v.content_disposition
             (fun f -> ("Content-Disposition", (String.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-checksum-algorithm", (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.cache_control
             (fun f -> ("Cache-Control", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket));
           Aws.Util.option_map v.a_c_l
             (fun f -> ("x-amz-acl", (ObjectCannedACL.to_json f)))])
    let of_json j =
      {
        a_c_l =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-acl")
             ObjectCannedACL.of_json);
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        cache_control =
          (Aws.Util.option_map (Aws.Json.lookup j "Cache-Control")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        content_disposition =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Disposition")
             String.of_json);
        content_encoding =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Encoding")
             String.of_json);
        content_language =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Language")
             String.of_json);
        content_type =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Type")
             String.of_json);
        copy_source =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "x-amz-copy-source")));
        copy_source_if_match =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-copy-source-if-match") String.of_json);
        copy_source_if_modified_since =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-copy-source-if-modified-since")
             DateTime.of_json);
        copy_source_if_none_match =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-copy-source-if-none-match")
             String.of_json);
        copy_source_if_unmodified_since =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-copy-source-if-unmodified-since")
             DateTime.of_json);
        expires =
          (Aws.Util.option_map (Aws.Json.lookup j "Expires") DateTime.of_json);
        grant_full_control =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-full-control")
             String.of_json);
        grant_read =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-read")
             String.of_json);
        grant_read_a_c_p =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-read-acp")
             String.of_json);
        grant_write_a_c_p =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-grant-write-acp")
             String.of_json);
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        metadata =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-meta-")
             Metadata.of_json);
        metadata_directive =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-metadata-directive")
             MetadataDirective.of_json);
        tagging_directive =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-tagging-directive")
             TaggingDirective.of_json);
        server_side_encryption =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption")
             ServerSideEncryption.of_json);
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-storage-class")
             StorageClass.of_json);
        website_redirect_location =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-website-redirect-location")
             String.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-customer-key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-aws-kms-key-id")
             String.of_json);
        s_s_e_k_m_s_encryption_context =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-context")
             String.of_json);
        bucket_key_enabled =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-bucket-key-enabled")
             Boolean.of_json);
        copy_source_s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-copy-source-server-side-encryption-customer-algorithm")
             String.of_json);
        copy_source_s_s_e_customer_key =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-copy-source-server-side-encryption-customer-key")
             String.of_json);
        copy_source_s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-copy-source-server-side-encryption-customer-key-MD5")
             String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        tagging =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-tagging")
             String.of_json);
        object_lock_mode =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-object-lock-mode")
             ObjectLockMode.of_json);
        object_lock_retain_until_date =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-object-lock-retain-until-date")
             DateTime.of_json);
        object_lock_legal_hold_status =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-object-lock-legal-hold")
             ObjectLockLegalHoldStatus.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        expected_source_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-source-expected-bucket-owner")
             String.of_json)
      }
  end
module StorageClassAnalysis =
  struct
    type t = {
      data_export: StorageClassAnalysisDataExport.t option }
    let make ?data_export  () = { data_export }
    let parse xml =
      Some
        {
          data_export =
            (Aws.Util.option_bind (Aws.Xml.member "DataExport" xml)
               StorageClassAnalysisDataExport.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.data_export
              (fun f ->
                 Aws.Query.Pair
                   ("DataExport",
                     (StorageClassAnalysisDataExport.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.data_export
              (fun f ->
                 ("DataExport", (StorageClassAnalysisDataExport.to_json f)))])
    let of_json j =
      {
        data_export =
          (Aws.Util.option_map (Aws.Json.lookup j "DataExport")
             StorageClassAnalysisDataExport.of_json)
      }
  end
module AnalyticsConfiguration =
  struct
    type t =
      {
      id: String.t ;
      filter: AnalyticsFilter.t option ;
      storage_class_analysis: StorageClassAnalysis.t }
    let make ~id  ?filter  ~storage_class_analysis  () =
      { id; filter; storage_class_analysis }
    let parse xml =
      Some
        {
          id =
            (Aws.Xml.required "Id"
               (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse));
          filter =
            (Aws.Util.option_bind (Aws.Xml.member "Filter" xml)
               AnalyticsFilter.parse);
          storage_class_analysis =
            (Aws.Xml.required "StorageClassAnalysis"
               (Aws.Util.option_bind
                  (Aws.Xml.member "StorageClassAnalysis" xml)
                  StorageClassAnalysis.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("StorageClassAnalysis",
                   (StorageClassAnalysis.to_query v.storage_class_analysis)));
           Aws.Util.option_map v.filter
             (fun f ->
                Aws.Query.Pair ("Filter", (AnalyticsFilter.to_query f)));
           Some (Aws.Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("StorageClassAnalysis",
                (StorageClassAnalysis.to_json v.storage_class_analysis));
           Aws.Util.option_map v.filter
             (fun f -> ("Filter", (AnalyticsFilter.to_json f)));
           Some ("Id", (String.to_json v.id))])
    let of_json j =
      {
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")));
        filter =
          (Aws.Util.option_map (Aws.Json.lookup j "Filter")
             AnalyticsFilter.of_json);
        storage_class_analysis =
          (StorageClassAnalysis.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "StorageClassAnalysis")))
      }
  end
module PutBucketAnalyticsConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      id: String.t ;
      analytics_configuration: AnalyticsConfiguration.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~id  ~analytics_configuration  ?expected_bucket_owner 
      () = { bucket; id; analytics_configuration; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          id =
            (Aws.Xml.required "id"
               (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse));
          analytics_configuration =
            (Aws.Xml.required "AnalyticsConfiguration"
               (Aws.Util.option_bind
                  (Aws.Xml.member "AnalyticsConfiguration" xml)
                  AnalyticsConfiguration.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("AnalyticsConfiguration",
                  (AnalyticsConfiguration.to_query v.analytics_configuration)));
           Some (Aws.Query.Pair ("id", (String.to_query v.id)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some
             ("AnalyticsConfiguration",
               (AnalyticsConfiguration.to_json v.analytics_configuration));
           Some ("id", (String.to_json v.id));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id")));
        analytics_configuration =
          (AnalyticsConfiguration.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "AnalyticsConfiguration")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module BucketVersioningStatus =
  struct
    type t =
      | Enabled 
      | Suspended 
    let str_to_t = [("Suspended", Suspended); ("Enabled", Enabled)]
    let t_to_str = [(Suspended, "Suspended"); (Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module GetBucketVersioningOutput =
  struct
    type t =
      {
      status: BucketVersioningStatus.t option ;
      m_f_a_delete: MFADeleteStatus.t option }
    let make ?status  ?m_f_a_delete  () = { status; m_f_a_delete }
    let parse xml =
      Some
        {
          status =
            (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
               BucketVersioningStatus.parse);
          m_f_a_delete =
            (Aws.Util.option_bind (Aws.Xml.member "MfaDelete" xml)
               MFADeleteStatus.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.m_f_a_delete
              (fun f ->
                 Aws.Query.Pair ("MfaDelete", (MFADeleteStatus.to_query f)));
           Aws.Util.option_map v.status
             (fun f ->
                Aws.Query.Pair
                  ("Status", (BucketVersioningStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.m_f_a_delete
              (fun f -> ("MfaDelete", (MFADeleteStatus.to_json f)));
           Aws.Util.option_map v.status
             (fun f -> ("Status", (BucketVersioningStatus.to_json f)))])
    let of_json j =
      {
        status =
          (Aws.Util.option_map (Aws.Json.lookup j "Status")
             BucketVersioningStatus.of_json);
        m_f_a_delete =
          (Aws.Util.option_map (Aws.Json.lookup j "MfaDelete")
             MFADeleteStatus.of_json)
      }
  end
module NoSuchUpload =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])
    let to_json v = `Assoc (Aws.Util.list_filter_opt [])
    let of_json j = ()
  end
module GetBucketLoggingOutput =
  struct
    type t = {
      logging_enabled: LoggingEnabled.t option }
    let make ?logging_enabled  () = { logging_enabled }
    let parse xml =
      Some
        {
          logging_enabled =
            (Aws.Util.option_bind (Aws.Xml.member "LoggingEnabled" xml)
               LoggingEnabled.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.logging_enabled
              (fun f ->
                 Aws.Query.Pair
                   ("LoggingEnabled", (LoggingEnabled.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.logging_enabled
              (fun f -> ("LoggingEnabled", (LoggingEnabled.to_json f)))])
    let of_json j =
      {
        logging_enabled =
          (Aws.Util.option_map (Aws.Json.lookup j "LoggingEnabled")
             LoggingEnabled.of_json)
      }
  end
module PutObjectLockConfigurationOutput =
  struct
    type t = {
      request_charged: RequestCharged.t option }
    let make ?request_charged  () = { request_charged }
    let parse xml =
      Some
        {
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)))])
    let of_json j =
      {
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module ListBucketInventoryConfigurationsOutput =
  struct
    type t =
      {
      continuation_token: String.t option ;
      inventory_configuration_list: InventoryConfigurationList.t ;
      is_truncated: Boolean.t option ;
      next_continuation_token: String.t option }
    let make ?continuation_token  ?(inventory_configuration_list= []) 
      ?is_truncated  ?next_continuation_token  () =
      {
        continuation_token;
        inventory_configuration_list;
        is_truncated;
        next_continuation_token
      }
    let parse xml =
      Some
        {
          continuation_token =
            (Aws.Util.option_bind (Aws.Xml.member "ContinuationToken" xml)
               String.parse);
          inventory_configuration_list =
            (Aws.Util.of_option [] (InventoryConfigurationList.parse xml));
          is_truncated =
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml)
               Boolean.parse);
          next_continuation_token =
            (Aws.Util.option_bind
               (Aws.Xml.member "NextContinuationToken" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.next_continuation_token
              (fun f ->
                 Aws.Query.Pair
                   ("NextContinuationToken", (String.to_query f)));
           Aws.Util.option_map v.is_truncated
             (fun f -> Aws.Query.Pair ("IsTruncated", (Boolean.to_query f)));
           Some
             (Aws.Query.Pair
                ("InventoryConfiguration",
                  (InventoryConfigurationList.to_query
                     v.inventory_configuration_list)));
           Aws.Util.option_map v.continuation_token
             (fun f ->
                Aws.Query.Pair ("ContinuationToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.next_continuation_token
              (fun f -> ("NextContinuationToken", (String.to_json f)));
           Aws.Util.option_map v.is_truncated
             (fun f -> ("IsTruncated", (Boolean.to_json f)));
           Some
             ("InventoryConfiguration",
               (InventoryConfigurationList.to_json
                  v.inventory_configuration_list));
           Aws.Util.option_map v.continuation_token
             (fun f -> ("ContinuationToken", (String.to_json f)))])
    let of_json j =
      {
        continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "ContinuationToken")
             String.of_json);
        inventory_configuration_list =
          (InventoryConfigurationList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "InventoryConfiguration")));
        is_truncated =
          (Aws.Util.option_map (Aws.Json.lookup j "IsTruncated")
             Boolean.of_json);
        next_continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "NextContinuationToken")
             String.of_json)
      }
  end
module UploadPartCopyRequest =
  struct
    type t =
      {
      bucket: String.t ;
      copy_source: String.t ;
      copy_source_if_match: String.t option ;
      copy_source_if_modified_since: DateTime.t option ;
      copy_source_if_none_match: String.t option ;
      copy_source_if_unmodified_since: DateTime.t option ;
      copy_source_range: String.t option ;
      key: String.t ;
      part_number: Integer.t ;
      upload_id: String.t ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      copy_source_s_s_e_customer_algorithm: String.t option ;
      copy_source_s_s_e_customer_key: String.t option ;
      copy_source_s_s_e_customer_key_m_d5: String.t option ;
      request_payer: RequestPayer.t option ;
      expected_bucket_owner: String.t option ;
      expected_source_bucket_owner: String.t option }
    let make ~bucket  ~copy_source  ?copy_source_if_match 
      ?copy_source_if_modified_since  ?copy_source_if_none_match 
      ?copy_source_if_unmodified_since  ?copy_source_range  ~key 
      ~part_number  ~upload_id  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key  ?s_s_e_customer_key_m_d5 
      ?copy_source_s_s_e_customer_algorithm  ?copy_source_s_s_e_customer_key 
      ?copy_source_s_s_e_customer_key_m_d5  ?request_payer 
      ?expected_bucket_owner  ?expected_source_bucket_owner  () =
      {
        bucket;
        copy_source;
        copy_source_if_match;
        copy_source_if_modified_since;
        copy_source_if_none_match;
        copy_source_if_unmodified_since;
        copy_source_range;
        key;
        part_number;
        upload_id;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        copy_source_s_s_e_customer_algorithm;
        copy_source_s_s_e_customer_key;
        copy_source_s_s_e_customer_key_m_d5;
        request_payer;
        expected_bucket_owner;
        expected_source_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          copy_source =
            (Aws.Xml.required "x-amz-copy-source"
               (Aws.Util.option_bind (Aws.Xml.member "x-amz-copy-source" xml)
                  String.parse));
          copy_source_if_match =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-copy-source-if-match" xml) String.parse);
          copy_source_if_modified_since =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-copy-source-if-modified-since" xml)
               DateTime.parse);
          copy_source_if_none_match =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-copy-source-if-none-match" xml)
               String.parse);
          copy_source_if_unmodified_since =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-copy-source-if-unmodified-since" xml)
               DateTime.parse);
          copy_source_range =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-copy-source-range" xml) String.parse);
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          part_number =
            (Aws.Xml.required "partNumber"
               (Aws.Util.option_bind (Aws.Xml.member "partNumber" xml)
                  Integer.parse));
          upload_id =
            (Aws.Xml.required "uploadId"
               (Aws.Util.option_bind (Aws.Xml.member "uploadId" xml)
                  String.parse));
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-customer-key"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          copy_source_s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-copy-source-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          copy_source_s_s_e_customer_key =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-copy-source-server-side-encryption-customer-key" xml)
               String.parse);
          copy_source_s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-copy-source-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          expected_source_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-source-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_source_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-source-expected-bucket-owner",
                     (String.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.copy_source_s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.copy_source_s_s_e_customer_key
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-server-side-encryption-customer-key",
                    (String.to_query f)));
           Aws.Util.option_map v.copy_source_s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Some (Aws.Query.Pair ("uploadId", (String.to_query v.upload_id)));
           Some
             (Aws.Query.Pair ("partNumber", (Integer.to_query v.part_number)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Aws.Util.option_map v.copy_source_range
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-range", (String.to_query f)));
           Aws.Util.option_map v.copy_source_if_unmodified_since
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-if-unmodified-since",
                    (DateTime.to_query f)));
           Aws.Util.option_map v.copy_source_if_none_match
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-if-none-match", (String.to_query f)));
           Aws.Util.option_map v.copy_source_if_modified_since
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-if-modified-since",
                    (DateTime.to_query f)));
           Aws.Util.option_map v.copy_source_if_match
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-copy-source-if-match", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("x-amz-copy-source", (String.to_query v.copy_source)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_source_bucket_owner
              (fun f ->
                 ("x-amz-source-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.copy_source_s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-copy-source-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.copy_source_s_s_e_customer_key
             (fun f ->
                ("x-amz-copy-source-server-side-encryption-customer-key",
                  (String.to_json f)));
           Aws.Util.option_map v.copy_source_s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-copy-source-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                ("x-amz-server-side-encryption-customer-key",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Some ("uploadId", (String.to_json v.upload_id));
           Some ("partNumber", (Integer.to_json v.part_number));
           Some ("Key", (String.to_json v.key));
           Aws.Util.option_map v.copy_source_range
             (fun f -> ("x-amz-copy-source-range", (String.to_json f)));
           Aws.Util.option_map v.copy_source_if_unmodified_since
             (fun f ->
                ("x-amz-copy-source-if-unmodified-since",
                  (DateTime.to_json f)));
           Aws.Util.option_map v.copy_source_if_none_match
             (fun f ->
                ("x-amz-copy-source-if-none-match", (String.to_json f)));
           Aws.Util.option_map v.copy_source_if_modified_since
             (fun f ->
                ("x-amz-copy-source-if-modified-since", (DateTime.to_json f)));
           Aws.Util.option_map v.copy_source_if_match
             (fun f -> ("x-amz-copy-source-if-match", (String.to_json f)));
           Some ("x-amz-copy-source", (String.to_json v.copy_source));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        copy_source =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "x-amz-copy-source")));
        copy_source_if_match =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-copy-source-if-match") String.of_json);
        copy_source_if_modified_since =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-copy-source-if-modified-since")
             DateTime.of_json);
        copy_source_if_none_match =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-copy-source-if-none-match")
             String.of_json);
        copy_source_if_unmodified_since =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-copy-source-if-unmodified-since")
             DateTime.of_json);
        copy_source_range =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-copy-source-range")
             String.of_json);
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        part_number =
          (Integer.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "partNumber")));
        upload_id =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "uploadId")));
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-customer-key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        copy_source_s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-copy-source-server-side-encryption-customer-algorithm")
             String.of_json);
        copy_source_s_s_e_customer_key =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-copy-source-server-side-encryption-customer-key")
             String.of_json);
        copy_source_s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-copy-source-server-side-encryption-customer-key-MD5")
             String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        expected_source_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-source-expected-bucket-owner")
             String.of_json)
      }
  end
module ObjectIdentifierList =
  struct
    type t = ObjectIdentifier.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map ObjectIdentifier.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list ObjectIdentifier.to_query v
    let to_json v = `List (List.map ObjectIdentifier.to_json v)
    let of_json j = Aws.Json.to_list ObjectIdentifier.of_json j
  end
module ObjectVersion =
  struct
    type t =
      {
      e_tag: String.t option ;
      checksum_algorithm: ChecksumAlgorithmList.t ;
      size: Integer.t option ;
      storage_class: ObjectVersionStorageClass.t option ;
      key: String.t option ;
      version_id: String.t option ;
      is_latest: Boolean.t option ;
      last_modified: DateTime.t option ;
      owner: Owner.t option ;
      restore_status: RestoreStatus.t option }
    let make ?e_tag  ?(checksum_algorithm= [])  ?size  ?storage_class  ?key 
      ?version_id  ?is_latest  ?last_modified  ?owner  ?restore_status  () =
      {
        e_tag;
        checksum_algorithm;
        size;
        storage_class;
        key;
        version_id;
        is_latest;
        last_modified;
        owner;
        restore_status
      }
    let parse xml =
      Some
        {
          e_tag =
            (Aws.Util.option_bind (Aws.Xml.member "ETag" xml) String.parse);
          checksum_algorithm =
            (Aws.Util.of_option [] (ChecksumAlgorithmList.parse xml));
          size =
            (Aws.Util.option_bind (Aws.Xml.member "Size" xml) Integer.parse);
          storage_class =
            (Aws.Util.option_bind (Aws.Xml.member "StorageClass" xml)
               ObjectVersionStorageClass.parse);
          key =
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "VersionId" xml)
               String.parse);
          is_latest =
            (Aws.Util.option_bind (Aws.Xml.member "IsLatest" xml)
               Boolean.parse);
          last_modified =
            (Aws.Util.option_bind (Aws.Xml.member "LastModified" xml)
               DateTime.parse);
          owner =
            (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) Owner.parse);
          restore_status =
            (Aws.Util.option_bind (Aws.Xml.member "RestoreStatus" xml)
               RestoreStatus.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.restore_status
              (fun f ->
                 Aws.Query.Pair ("RestoreStatus", (RestoreStatus.to_query f)));
           Aws.Util.option_map v.owner
             (fun f -> Aws.Query.Pair ("Owner", (Owner.to_query f)));
           Aws.Util.option_map v.last_modified
             (fun f -> Aws.Query.Pair ("LastModified", (DateTime.to_query f)));
           Aws.Util.option_map v.is_latest
             (fun f -> Aws.Query.Pair ("IsLatest", (Boolean.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("VersionId", (String.to_query f)));
           Aws.Util.option_map v.key
             (fun f -> Aws.Query.Pair ("Key", (String.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair
                  ("StorageClass", (ObjectVersionStorageClass.to_query f)));
           Aws.Util.option_map v.size
             (fun f -> Aws.Query.Pair ("Size", (Integer.to_query f)));
           Some
             (Aws.Query.Pair
                ("ChecksumAlgorithm.member",
                  (ChecksumAlgorithmList.to_query v.checksum_algorithm)));
           Aws.Util.option_map v.e_tag
             (fun f -> Aws.Query.Pair ("ETag", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.restore_status
              (fun f -> ("RestoreStatus", (RestoreStatus.to_json f)));
           Aws.Util.option_map v.owner
             (fun f -> ("Owner", (Owner.to_json f)));
           Aws.Util.option_map v.last_modified
             (fun f -> ("LastModified", (DateTime.to_json f)));
           Aws.Util.option_map v.is_latest
             (fun f -> ("IsLatest", (Boolean.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("VersionId", (String.to_json f)));
           Aws.Util.option_map v.key (fun f -> ("Key", (String.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                ("StorageClass", (ObjectVersionStorageClass.to_json f)));
           Aws.Util.option_map v.size
             (fun f -> ("Size", (Integer.to_json f)));
           Some
             ("ChecksumAlgorithm",
               (ChecksumAlgorithmList.to_json v.checksum_algorithm));
           Aws.Util.option_map v.e_tag
             (fun f -> ("ETag", (String.to_json f)))])
    let of_json j =
      {
        e_tag =
          (Aws.Util.option_map (Aws.Json.lookup j "ETag") String.of_json);
        checksum_algorithm =
          (ChecksumAlgorithmList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "ChecksumAlgorithm")));
        size =
          (Aws.Util.option_map (Aws.Json.lookup j "Size") Integer.of_json);
        storage_class =
          (Aws.Util.option_map (Aws.Json.lookup j "StorageClass")
             ObjectVersionStorageClass.of_json);
        key = (Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "VersionId") String.of_json);
        is_latest =
          (Aws.Util.option_map (Aws.Json.lookup j "IsLatest") Boolean.of_json);
        last_modified =
          (Aws.Util.option_map (Aws.Json.lookup j "LastModified")
             DateTime.of_json);
        owner =
          (Aws.Util.option_map (Aws.Json.lookup j "Owner") Owner.of_json);
        restore_status =
          (Aws.Util.option_map (Aws.Json.lookup j "RestoreStatus")
             RestoreStatus.of_json)
      }
  end
module ObjectVersionList =
  struct
    type t = ObjectVersion.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map ObjectVersion.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list ObjectVersion.to_query v
    let to_json v = `List (List.map ObjectVersion.to_json v)
    let of_json j = Aws.Json.to_list ObjectVersion.of_json j
  end
module UploadPartOutput =
  struct
    type t =
      {
      server_side_encryption: ServerSideEncryption.t option ;
      e_tag: String.t option ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      bucket_key_enabled: Boolean.t option ;
      request_charged: RequestCharged.t option }
    let make ?server_side_encryption  ?e_tag  ?checksum_c_r_c32 
      ?checksum_c_r_c32_c  ?checksum_s_h_a1  ?checksum_s_h_a256 
      ?s_s_e_customer_algorithm  ?s_s_e_customer_key_m_d5 
      ?s_s_e_k_m_s_key_id  ?bucket_key_enabled  ?request_charged  () =
      {
        server_side_encryption;
        e_tag;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        bucket_key_enabled;
        request_charged
      }
    let parse xml =
      Some
        {
          server_side_encryption =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          e_tag =
            (Aws.Util.option_bind (Aws.Xml.member "ETag" xml) String.parse);
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-crc32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-crc32c" xml) String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-sha1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-sha256" xml) String.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          s_s_e_k_m_s_key_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-aws-kms-key-id"
                  xml) String.parse);
          bucket_key_enabled =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-bucket-key-enabled" xml)
               Boolean.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-bucket-key-enabled",
                    (Boolean.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-sha1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32c", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f ->
                Aws.Query.Pair ("x-amz-checksum-crc32", (String.to_query f)));
           Aws.Util.option_map v.e_tag
             (fun f -> Aws.Query.Pair ("ETag", (String.to_query f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                ("x-amz-server-side-encryption-bucket-key-enabled",
                  (Boolean.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                ("x-amz-server-side-encryption-aws-kms-key-id",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f -> ("x-amz-checksum-sha256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("x-amz-checksum-sha1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("x-amz-checksum-crc32c", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("x-amz-checksum-crc32", (String.to_json f)));
           Aws.Util.option_map v.e_tag
             (fun f -> ("ETag", (String.to_json f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                ("x-amz-server-side-encryption",
                  (ServerSideEncryption.to_json f)))])
    let of_json j =
      {
        server_side_encryption =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption")
             ServerSideEncryption.of_json);
        e_tag =
          (Aws.Util.option_map (Aws.Json.lookup j "ETag") String.of_json);
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-crc32c")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-sha256")
             String.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-aws-kms-key-id")
             String.of_json);
        bucket_key_enabled =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-bucket-key-enabled")
             Boolean.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module Delete =
  struct
    type t = {
      objects: ObjectIdentifierList.t ;
      quiet: Boolean.t option }
    let make ~objects  ?quiet  () = { objects; quiet }
    let parse xml =
      Some
        {
          objects =
            (Aws.Xml.required "Object" (ObjectIdentifierList.parse xml));
          quiet =
            (Aws.Util.option_bind (Aws.Xml.member "Quiet" xml) Boolean.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.quiet
              (fun f -> Aws.Query.Pair ("Quiet", (Boolean.to_query f)));
           Some
             (Aws.Query.Pair
                ("Object", (ObjectIdentifierList.to_query v.objects)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.quiet
              (fun f -> ("Quiet", (Boolean.to_json f)));
           Some ("Object", (ObjectIdentifierList.to_json v.objects))])
    let of_json j =
      {
        objects =
          (ObjectIdentifierList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Object")));
        quiet =
          (Aws.Util.option_map (Aws.Json.lookup j "Quiet") Boolean.of_json)
      }
  end
module DeleteObjectsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      delete: Delete.t ;
      m_f_a: String.t option ;
      request_payer: RequestPayer.t option ;
      bypass_governance_retention: Boolean.t option ;
      expected_bucket_owner: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option }
    let make ~bucket  ~delete  ?m_f_a  ?request_payer 
      ?bypass_governance_retention  ?expected_bucket_owner 
      ?checksum_algorithm  () =
      {
        bucket;
        delete;
        m_f_a;
        request_payer;
        bypass_governance_retention;
        expected_bucket_owner;
        checksum_algorithm
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          delete =
            (Aws.Xml.required "Delete"
               (Aws.Util.option_bind (Aws.Xml.member "Delete" xml)
                  Delete.parse));
          m_f_a =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-mfa" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          bypass_governance_retention =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-bypass-governance-retention" xml)
               Boolean.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_algorithm
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-sdk-checksum-algorithm",
                     (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.bypass_governance_retention
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-bypass-governance-retention", (Boolean.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.m_f_a
             (fun f -> Aws.Query.Pair ("x-amz-mfa", (String.to_query f)));
           Some (Aws.Query.Pair ("Delete", (Delete.to_query v.delete)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_algorithm
              (fun f ->
                 ("x-amz-sdk-checksum-algorithm",
                   (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.bypass_governance_retention
             (fun f ->
                ("x-amz-bypass-governance-retention", (Boolean.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.m_f_a
             (fun f -> ("x-amz-mfa", (String.to_json f)));
           Some ("Delete", (Delete.to_json v.delete));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        delete =
          (Delete.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Delete")));
        m_f_a =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-mfa") String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        bypass_governance_retention =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-bypass-governance-retention")
             Boolean.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json)
      }
  end
module ListBucketIntelligentTieringConfigurationsOutput =
  struct
    type t =
      {
      is_truncated: Boolean.t option ;
      continuation_token: String.t option ;
      next_continuation_token: String.t option ;
      intelligent_tiering_configuration_list:
        IntelligentTieringConfigurationList.t }
    let make ?is_truncated  ?continuation_token  ?next_continuation_token 
      ?(intelligent_tiering_configuration_list= [])  () =
      {
        is_truncated;
        continuation_token;
        next_continuation_token;
        intelligent_tiering_configuration_list
      }
    let parse xml =
      Some
        {
          is_truncated =
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml)
               Boolean.parse);
          continuation_token =
            (Aws.Util.option_bind (Aws.Xml.member "ContinuationToken" xml)
               String.parse);
          next_continuation_token =
            (Aws.Util.option_bind
               (Aws.Xml.member "NextContinuationToken" xml) String.parse);
          intelligent_tiering_configuration_list =
            (Aws.Util.of_option []
               (IntelligentTieringConfigurationList.parse xml))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("IntelligentTieringConfiguration",
                   (IntelligentTieringConfigurationList.to_query
                      v.intelligent_tiering_configuration_list)));
           Aws.Util.option_map v.next_continuation_token
             (fun f ->
                Aws.Query.Pair ("NextContinuationToken", (String.to_query f)));
           Aws.Util.option_map v.continuation_token
             (fun f ->
                Aws.Query.Pair ("ContinuationToken", (String.to_query f)));
           Aws.Util.option_map v.is_truncated
             (fun f -> Aws.Query.Pair ("IsTruncated", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("IntelligentTieringConfiguration",
                (IntelligentTieringConfigurationList.to_json
                   v.intelligent_tiering_configuration_list));
           Aws.Util.option_map v.next_continuation_token
             (fun f -> ("NextContinuationToken", (String.to_json f)));
           Aws.Util.option_map v.continuation_token
             (fun f -> ("ContinuationToken", (String.to_json f)));
           Aws.Util.option_map v.is_truncated
             (fun f -> ("IsTruncated", (Boolean.to_json f)))])
    let of_json j =
      {
        is_truncated =
          (Aws.Util.option_map (Aws.Json.lookup j "IsTruncated")
             Boolean.of_json);
        continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "ContinuationToken")
             String.of_json);
        next_continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "NextContinuationToken")
             String.of_json);
        intelligent_tiering_configuration_list =
          (IntelligentTieringConfigurationList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "IntelligentTieringConfiguration")))
      }
  end
module GetBucketRequestPaymentRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetObjectAclOutput =
  struct
    type t =
      {
      owner: Owner.t option ;
      grants: Grants.t ;
      request_charged: RequestCharged.t option }
    let make ?owner  ?(grants= [])  ?request_charged  () =
      { owner; grants; request_charged }
    let parse xml =
      Some
        {
          owner =
            (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) Owner.parse);
          grants =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "AccessControlList" xml)
                  Grants.parse));
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Some
             (Aws.Query.Pair
                ("AccessControlList", (Grants.to_query v.grants)));
           Aws.Util.option_map v.owner
             (fun f -> Aws.Query.Pair ("Owner", (Owner.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Some ("AccessControlList", (Grants.to_json v.grants));
           Aws.Util.option_map v.owner
             (fun f -> ("Owner", (Owner.to_json f)))])
    let of_json j =
      {
        owner =
          (Aws.Util.option_map (Aws.Json.lookup j "Owner") Owner.of_json);
        grants =
          (Grants.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AccessControlList")));
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module GetBucketAclOutput =
  struct
    type t = {
      owner: Owner.t option ;
      grants: Grants.t }
    let make ?owner  ?(grants= [])  () = { owner; grants }
    let parse xml =
      Some
        {
          owner =
            (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) Owner.parse);
          grants =
            (Aws.Util.of_option []
               (Aws.Util.option_bind (Aws.Xml.member "AccessControlList" xml)
                  Grants.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("AccessControlList", (Grants.to_query v.grants)));
           Aws.Util.option_map v.owner
             (fun f -> Aws.Query.Pair ("Owner", (Owner.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("AccessControlList", (Grants.to_json v.grants));
           Aws.Util.option_map v.owner
             (fun f -> ("Owner", (Owner.to_json f)))])
    let of_json j =
      {
        owner =
          (Aws.Util.option_map (Aws.Json.lookup j "Owner") Owner.of_json);
        grants =
          (Grants.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "AccessControlList")))
      }
  end
module PutObjectTaggingRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      version_id: String.t option ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      tagging: Tagging.t ;
      expected_bucket_owner: String.t option ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ~key  ?version_id  ?content_m_d5  ?checksum_algorithm 
      ~tagging  ?expected_bucket_owner  ?request_payer  () =
      {
        bucket;
        key;
        version_id;
        content_m_d5;
        checksum_algorithm;
        tagging;
        expected_bucket_owner;
        request_payer
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          tagging =
            (Aws.Xml.required "Tagging"
               (Aws.Util.option_bind (Aws.Xml.member "Tagging" xml)
                  Tagging.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_payer
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Tagging", (Tagging.to_query v.tagging)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_payer
              (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Tagging", (Tagging.to_json v.tagging));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        tagging =
          (Tagging.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Tagging")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json)
      }
  end
module CompleteMultipartUploadOutput =
  struct
    type t =
      {
      location: String.t option ;
      bucket: String.t option ;
      key: String.t option ;
      expiration: String.t option ;
      e_tag: String.t option ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      version_id: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      bucket_key_enabled: Boolean.t option ;
      request_charged: RequestCharged.t option }
    let make ?location  ?bucket  ?key  ?expiration  ?e_tag  ?checksum_c_r_c32
       ?checksum_c_r_c32_c  ?checksum_s_h_a1  ?checksum_s_h_a256 
      ?server_side_encryption  ?version_id  ?s_s_e_k_m_s_key_id 
      ?bucket_key_enabled  ?request_charged  () =
      {
        location;
        bucket;
        key;
        expiration;
        e_tag;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256;
        server_side_encryption;
        version_id;
        s_s_e_k_m_s_key_id;
        bucket_key_enabled;
        request_charged
      }
    let parse xml =
      Some
        {
          location =
            (Aws.Util.option_bind (Aws.Xml.member "Location" xml)
               String.parse);
          bucket =
            (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml) String.parse);
          key =
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse);
          expiration =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-expiration" xml)
               String.parse);
          e_tag =
            (Aws.Util.option_bind (Aws.Xml.member "ETag" xml) String.parse);
          checksum_c_r_c32 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumCRC32C" xml)
               String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind (Aws.Xml.member "ChecksumSHA256" xml)
               String.parse);
          server_side_encryption =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-version-id" xml)
               String.parse);
          s_s_e_k_m_s_key_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-aws-kms-key-id"
                  xml) String.parse);
          bucket_key_enabled =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-bucket-key-enabled" xml)
               Boolean.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-bucket-key-enabled",
                    (Boolean.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f ->
                Aws.Query.Pair ("x-amz-version-id", (String.to_query f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f -> Aws.Query.Pair ("ChecksumSHA256", (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> Aws.Query.Pair ("ChecksumSHA1", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> Aws.Query.Pair ("ChecksumCRC32C", (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> Aws.Query.Pair ("ChecksumCRC32", (String.to_query f)));
           Aws.Util.option_map v.e_tag
             (fun f -> Aws.Query.Pair ("ETag", (String.to_query f)));
           Aws.Util.option_map v.expiration
             (fun f ->
                Aws.Query.Pair ("x-amz-expiration", (String.to_query f)));
           Aws.Util.option_map v.key
             (fun f -> Aws.Query.Pair ("Key", (String.to_query f)));
           Aws.Util.option_map v.bucket
             (fun f -> Aws.Query.Pair ("Bucket", (String.to_query f)));
           Aws.Util.option_map v.location
             (fun f -> Aws.Query.Pair ("Location", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                ("x-amz-server-side-encryption-bucket-key-enabled",
                  (Boolean.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                ("x-amz-server-side-encryption-aws-kms-key-id",
                  (String.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("x-amz-version-id", (String.to_json f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                ("x-amz-server-side-encryption",
                  (ServerSideEncryption.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f -> ("ChecksumSHA256", (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f -> ("ChecksumSHA1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f -> ("ChecksumCRC32C", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f -> ("ChecksumCRC32", (String.to_json f)));
           Aws.Util.option_map v.e_tag
             (fun f -> ("ETag", (String.to_json f)));
           Aws.Util.option_map v.expiration
             (fun f -> ("x-amz-expiration", (String.to_json f)));
           Aws.Util.option_map v.key (fun f -> ("Key", (String.to_json f)));
           Aws.Util.option_map v.bucket
             (fun f -> ("Bucket", (String.to_json f)));
           Aws.Util.option_map v.location
             (fun f -> ("Location", (String.to_json f)))])
    let of_json j =
      {
        location =
          (Aws.Util.option_map (Aws.Json.lookup j "Location") String.of_json);
        bucket =
          (Aws.Util.option_map (Aws.Json.lookup j "Bucket") String.of_json);
        key = (Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json);
        expiration =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-expiration")
             String.of_json);
        e_tag =
          (Aws.Util.option_map (Aws.Json.lookup j "ETag") String.of_json);
        checksum_c_r_c32 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumCRC32C")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map (Aws.Json.lookup j "ChecksumSHA256")
             String.of_json);
        server_side_encryption =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption")
             ServerSideEncryption.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-version-id")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-aws-kms-key-id")
             String.of_json);
        bucket_key_enabled =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-bucket-key-enabled")
             Boolean.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module MFADelete =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("Disabled", Disabled); ("Enabled", Enabled)]
    let t_to_str = [(Disabled, "Disabled"); (Enabled, "Enabled")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module DeleteBucketReplicationRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module DeleteObjectOutput =
  struct
    type t =
      {
      delete_marker: Boolean.t option ;
      version_id: String.t option ;
      request_charged: RequestCharged.t option }
    let make ?delete_marker  ?version_id  ?request_charged  () =
      { delete_marker; version_id; request_charged }
    let parse xml =
      Some
        {
          delete_marker =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-delete-marker" xml)
               Boolean.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-version-id" xml)
               String.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f ->
                Aws.Query.Pair ("x-amz-version-id", (String.to_query f)));
           Aws.Util.option_map v.delete_marker
             (fun f ->
                Aws.Query.Pair ("x-amz-delete-marker", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("x-amz-version-id", (String.to_json f)));
           Aws.Util.option_map v.delete_marker
             (fun f -> ("x-amz-delete-marker", (Boolean.to_json f)))])
    let of_json j =
      {
        delete_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-delete-marker")
             Boolean.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-version-id")
             String.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module OutputLocation =
  struct
    type t = {
      s3: S3Location.t option }
    let make ?s3  () = { s3 }
    let parse xml =
      Some
        {
          s3 =
            (Aws.Util.option_bind (Aws.Xml.member "S3" xml) S3Location.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.s3
              (fun f -> Aws.Query.Pair ("S3", (S3Location.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.s3
              (fun f -> ("S3", (S3Location.to_json f)))])
    let of_json j =
      {
        s3 =
          (Aws.Util.option_map (Aws.Json.lookup j "S3") S3Location.of_json)
      }
  end
module PutObjectLockConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      object_lock_configuration: ObjectLockConfiguration.t option ;
      request_payer: RequestPayer.t option ;
      token: String.t option ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?object_lock_configuration  ?request_payer  ?token 
      ?content_m_d5  ?checksum_algorithm  ?expected_bucket_owner  () =
      {
        bucket;
        object_lock_configuration;
        request_payer;
        token;
        content_m_d5;
        checksum_algorithm;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          object_lock_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "ObjectLockConfiguration" xml)
               ObjectLockConfiguration.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          token =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-bucket-object-lock-token" xml)
               String.parse);
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Aws.Util.option_map v.token
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-bucket-object-lock-token", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.object_lock_configuration
             (fun f ->
                Aws.Query.Pair
                  ("ObjectLockConfiguration",
                    (ObjectLockConfiguration.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Aws.Util.option_map v.token
             (fun f -> ("x-amz-bucket-object-lock-token", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.object_lock_configuration
             (fun f ->
                ("ObjectLockConfiguration",
                  (ObjectLockConfiguration.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        object_lock_configuration =
          (Aws.Util.option_map (Aws.Json.lookup j "ObjectLockConfiguration")
             ObjectLockConfiguration.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        token =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-bucket-object-lock-token")
             String.of_json);
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetBucketInventoryConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      id: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~id  ?expected_bucket_owner  () =
      { bucket; id; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          id =
            (Aws.Xml.required "id"
               (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("id", (String.to_query v.id)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("id", (String.to_json v.id));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module DeleteBucketMetricsConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      id: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~id  ?expected_bucket_owner  () =
      { bucket; id; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          id =
            (Aws.Xml.required "id"
               (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("id", (String.to_query v.id)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("id", (String.to_json v.id));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module NoSuchKey =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])
    let to_json v = `Assoc (Aws.Util.list_filter_opt [])
    let of_json j = ()
  end
module WriteGetObjectResponseRequest =
  struct
    type t =
      {
      request_route: String.t ;
      request_token: String.t ;
      body: Blob.t option ;
      status_code: Integer.t option ;
      error_code: String.t option ;
      error_message: String.t option ;
      accept_ranges: String.t option ;
      cache_control: String.t option ;
      content_disposition: String.t option ;
      content_encoding: String.t option ;
      content_language: String.t option ;
      content_length: Long.t option ;
      content_range: String.t option ;
      content_type: String.t option ;
      checksum_c_r_c32: String.t option ;
      checksum_c_r_c32_c: String.t option ;
      checksum_s_h_a1: String.t option ;
      checksum_s_h_a256: String.t option ;
      delete_marker: Boolean.t option ;
      e_tag: String.t option ;
      expires: DateTime.t option ;
      expiration: String.t option ;
      last_modified: DateTime.t option ;
      missing_meta: Integer.t option ;
      metadata: Metadata.t option ;
      object_lock_mode: ObjectLockMode.t option ;
      object_lock_legal_hold_status: ObjectLockLegalHoldStatus.t option ;
      object_lock_retain_until_date: DateTime.t option ;
      parts_count: Integer.t option ;
      replication_status: ReplicationStatus.t option ;
      request_charged: RequestCharged.t option ;
      restore: String.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      storage_class: StorageClass.t option ;
      tag_count: Integer.t option ;
      version_id: String.t option ;
      bucket_key_enabled: Boolean.t option }
    let make ~request_route  ~request_token  ?body  ?status_code  ?error_code
       ?error_message  ?accept_ranges  ?cache_control  ?content_disposition 
      ?content_encoding  ?content_language  ?content_length  ?content_range 
      ?content_type  ?checksum_c_r_c32  ?checksum_c_r_c32_c  ?checksum_s_h_a1
       ?checksum_s_h_a256  ?delete_marker  ?e_tag  ?expires  ?expiration 
      ?last_modified  ?missing_meta  ?metadata  ?object_lock_mode 
      ?object_lock_legal_hold_status  ?object_lock_retain_until_date 
      ?parts_count  ?replication_status  ?request_charged  ?restore 
      ?server_side_encryption  ?s_s_e_customer_algorithm  ?s_s_e_k_m_s_key_id
       ?s_s_e_customer_key_m_d5  ?storage_class  ?tag_count  ?version_id 
      ?bucket_key_enabled  () =
      {
        request_route;
        request_token;
        body;
        status_code;
        error_code;
        error_message;
        accept_ranges;
        cache_control;
        content_disposition;
        content_encoding;
        content_language;
        content_length;
        content_range;
        content_type;
        checksum_c_r_c32;
        checksum_c_r_c32_c;
        checksum_s_h_a1;
        checksum_s_h_a256;
        delete_marker;
        e_tag;
        expires;
        expiration;
        last_modified;
        missing_meta;
        metadata;
        object_lock_mode;
        object_lock_legal_hold_status;
        object_lock_retain_until_date;
        parts_count;
        replication_status;
        request_charged;
        restore;
        server_side_encryption;
        s_s_e_customer_algorithm;
        s_s_e_k_m_s_key_id;
        s_s_e_customer_key_m_d5;
        storage_class;
        tag_count;
        version_id;
        bucket_key_enabled
      }
    let parse xml =
      Some
        {
          request_route =
            (Aws.Xml.required "x-amz-request-route"
               (Aws.Util.option_bind
                  (Aws.Xml.member "x-amz-request-route" xml) String.parse));
          request_token =
            (Aws.Xml.required "x-amz-request-token"
               (Aws.Util.option_bind
                  (Aws.Xml.member "x-amz-request-token" xml) String.parse));
          body =
            (Aws.Util.option_bind (Aws.Xml.member "Body" xml) Blob.parse);
          status_code =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-fwd-status" xml)
               Integer.parse);
          error_code =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-fwd-error-code" xml)
               String.parse);
          error_message =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-error-message" xml) String.parse);
          accept_ranges =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-accept-ranges" xml)
               String.parse);
          cache_control =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-Cache-Control" xml)
               String.parse);
          content_disposition =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-Content-Disposition" xml)
               String.parse);
          content_encoding =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-Content-Encoding" xml)
               String.parse);
          content_language =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-Content-Language" xml)
               String.parse);
          content_length =
            (Aws.Util.option_bind (Aws.Xml.member "Content-Length" xml)
               Long.parse);
          content_range =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-Content-Range" xml)
               String.parse);
          content_type =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-Content-Type" xml)
               String.parse);
          checksum_c_r_c32 =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-checksum-crc32" xml)
               String.parse);
          checksum_c_r_c32_c =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-checksum-crc32c" xml)
               String.parse);
          checksum_s_h_a1 =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-checksum-sha1" xml)
               String.parse);
          checksum_s_h_a256 =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-checksum-sha256" xml)
               String.parse);
          delete_marker =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-delete-marker" xml)
               Boolean.parse);
          e_tag =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-ETag" xml) String.parse);
          expires =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-Expires" xml) DateTime.parse);
          expiration =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-expiration" xml)
               String.parse);
          last_modified =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-Last-Modified" xml)
               DateTime.parse);
          missing_meta =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-missing-meta" xml)
               Integer.parse);
          metadata =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-meta-" xml)
               Metadata.parse);
          object_lock_mode =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-object-lock-mode" xml)
               ObjectLockMode.parse);
          object_lock_legal_hold_status =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-fwd-header-x-amz-object-lock-legal-hold" xml)
               ObjectLockLegalHoldStatus.parse);
          object_lock_retain_until_date =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-fwd-header-x-amz-object-lock-retain-until-date" xml)
               DateTime.parse);
          parts_count =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-mp-parts-count" xml)
               Integer.parse);
          replication_status =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-replication-status"
                  xml) ReplicationStatus.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-request-charged" xml)
               RequestCharged.parse);
          restore =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-restore" xml)
               String.parse);
          server_side_encryption =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-fwd-header-x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-fwd-header-x-amz-server-side-encryption-customer-algorithm"
                  xml) String.parse);
          s_s_e_k_m_s_key_id =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-fwd-header-x-amz-server-side-encryption-aws-kms-key-id"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-fwd-header-x-amz-server-side-encryption-customer-key-MD5"
                  xml) String.parse);
          storage_class =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-storage-class" xml)
               StorageClass.parse);
          tag_count =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-tagging-count" xml)
               Integer.parse);
          version_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-fwd-header-x-amz-version-id" xml)
               String.parse);
          bucket_key_enabled =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-fwd-header-x-amz-server-side-encryption-bucket-key-enabled"
                  xml) Boolean.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.bucket_key_enabled
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-fwd-header-x-amz-server-side-encryption-bucket-key-enabled",
                     (Boolean.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-version-id", (String.to_query f)));
           Aws.Util.option_map v.tag_count
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-tagging-count",
                    (Integer.to_query f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-storage-class",
                    (StorageClass.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Aws.Util.option_map v.restore
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-restore", (String.to_query f)));
           Aws.Util.option_map v.request_charged
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-request-charged",
                    (RequestCharged.to_query f)));
           Aws.Util.option_map v.replication_status
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-replication-status",
                    (ReplicationStatus.to_query f)));
           Aws.Util.option_map v.parts_count
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-mp-parts-count",
                    (Integer.to_query f)));
           Aws.Util.option_map v.object_lock_retain_until_date
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-object-lock-retain-until-date",
                    (DateTime.to_query f)));
           Aws.Util.option_map v.object_lock_legal_hold_status
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-object-lock-legal-hold",
                    (ObjectLockLegalHoldStatus.to_query f)));
           Aws.Util.option_map v.object_lock_mode
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-object-lock-mode",
                    (ObjectLockMode.to_query f)));
           Aws.Util.option_map v.metadata
             (fun f -> Aws.Query.Pair ("x-amz-meta-", (Metadata.to_query f)));
           Aws.Util.option_map v.missing_meta
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-missing-meta",
                    (Integer.to_query f)));
           Aws.Util.option_map v.last_modified
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-Last-Modified", (DateTime.to_query f)));
           Aws.Util.option_map v.expiration
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-expiration", (String.to_query f)));
           Aws.Util.option_map v.expires
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-Expires", (DateTime.to_query f)));
           Aws.Util.option_map v.e_tag
             (fun f ->
                Aws.Query.Pair ("x-amz-fwd-header-ETag", (String.to_query f)));
           Aws.Util.option_map v.delete_marker
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-delete-marker",
                    (Boolean.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-checksum-sha256",
                    (String.to_query f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-checksum-sha1",
                    (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-checksum-crc32c",
                    (String.to_query f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-x-amz-checksum-crc32",
                    (String.to_query f)));
           Aws.Util.option_map v.content_type
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-Content-Type", (String.to_query f)));
           Aws.Util.option_map v.content_range
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-Content-Range", (String.to_query f)));
           Aws.Util.option_map v.content_length
             (fun f -> Aws.Query.Pair ("Content-Length", (Long.to_query f)));
           Aws.Util.option_map v.content_language
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-Content-Language", (String.to_query f)));
           Aws.Util.option_map v.content_encoding
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-Content-Encoding", (String.to_query f)));
           Aws.Util.option_map v.content_disposition
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-Content-Disposition",
                    (String.to_query f)));
           Aws.Util.option_map v.cache_control
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-Cache-Control", (String.to_query f)));
           Aws.Util.option_map v.accept_ranges
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-header-accept-ranges", (String.to_query f)));
           Aws.Util.option_map v.error_message
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-fwd-error-message", (String.to_query f)));
           Aws.Util.option_map v.error_code
             (fun f ->
                Aws.Query.Pair ("x-amz-fwd-error-code", (String.to_query f)));
           Aws.Util.option_map v.status_code
             (fun f ->
                Aws.Query.Pair ("x-amz-fwd-status", (Integer.to_query f)));
           Aws.Util.option_map v.body
             (fun f -> Aws.Query.Pair ("Body", (Blob.to_query f)));
           Some
             (Aws.Query.Pair
                ("x-amz-request-token", (String.to_query v.request_token)));
           Some
             (Aws.Query.Pair
                ("x-amz-request-route", (String.to_query v.request_route)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.bucket_key_enabled
              (fun f ->
                 ("x-amz-fwd-header-x-amz-server-side-encryption-bucket-key-enabled",
                   (Boolean.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f ->
                ("x-amz-fwd-header-x-amz-version-id", (String.to_json f)));
           Aws.Util.option_map v.tag_count
             (fun f ->
                ("x-amz-fwd-header-x-amz-tagging-count", (Integer.to_json f)));
           Aws.Util.option_map v.storage_class
             (fun f ->
                ("x-amz-fwd-header-x-amz-storage-class",
                  (StorageClass.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-fwd-header-x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                ("x-amz-fwd-header-x-amz-server-side-encryption-aws-kms-key-id",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-fwd-header-x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                ("x-amz-fwd-header-x-amz-server-side-encryption",
                  (ServerSideEncryption.to_json f)));
           Aws.Util.option_map v.restore
             (fun f -> ("x-amz-fwd-header-x-amz-restore", (String.to_json f)));
           Aws.Util.option_map v.request_charged
             (fun f ->
                ("x-amz-fwd-header-x-amz-request-charged",
                  (RequestCharged.to_json f)));
           Aws.Util.option_map v.replication_status
             (fun f ->
                ("x-amz-fwd-header-x-amz-replication-status",
                  (ReplicationStatus.to_json f)));
           Aws.Util.option_map v.parts_count
             (fun f ->
                ("x-amz-fwd-header-x-amz-mp-parts-count",
                  (Integer.to_json f)));
           Aws.Util.option_map v.object_lock_retain_until_date
             (fun f ->
                ("x-amz-fwd-header-x-amz-object-lock-retain-until-date",
                  (DateTime.to_json f)));
           Aws.Util.option_map v.object_lock_legal_hold_status
             (fun f ->
                ("x-amz-fwd-header-x-amz-object-lock-legal-hold",
                  (ObjectLockLegalHoldStatus.to_json f)));
           Aws.Util.option_map v.object_lock_mode
             (fun f ->
                ("x-amz-fwd-header-x-amz-object-lock-mode",
                  (ObjectLockMode.to_json f)));
           Aws.Util.option_map v.metadata
             (fun f -> ("x-amz-meta-", (Metadata.to_json f)));
           Aws.Util.option_map v.missing_meta
             (fun f ->
                ("x-amz-fwd-header-x-amz-missing-meta", (Integer.to_json f)));
           Aws.Util.option_map v.last_modified
             (fun f ->
                ("x-amz-fwd-header-Last-Modified", (DateTime.to_json f)));
           Aws.Util.option_map v.expiration
             (fun f ->
                ("x-amz-fwd-header-x-amz-expiration", (String.to_json f)));
           Aws.Util.option_map v.expires
             (fun f -> ("x-amz-fwd-header-Expires", (DateTime.to_json f)));
           Aws.Util.option_map v.e_tag
             (fun f -> ("x-amz-fwd-header-ETag", (String.to_json f)));
           Aws.Util.option_map v.delete_marker
             (fun f ->
                ("x-amz-fwd-header-x-amz-delete-marker", (Boolean.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a256
             (fun f ->
                ("x-amz-fwd-header-x-amz-checksum-sha256",
                  (String.to_json f)));
           Aws.Util.option_map v.checksum_s_h_a1
             (fun f ->
                ("x-amz-fwd-header-x-amz-checksum-sha1", (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32_c
             (fun f ->
                ("x-amz-fwd-header-x-amz-checksum-crc32c",
                  (String.to_json f)));
           Aws.Util.option_map v.checksum_c_r_c32
             (fun f ->
                ("x-amz-fwd-header-x-amz-checksum-crc32", (String.to_json f)));
           Aws.Util.option_map v.content_type
             (fun f -> ("x-amz-fwd-header-Content-Type", (String.to_json f)));
           Aws.Util.option_map v.content_range
             (fun f -> ("x-amz-fwd-header-Content-Range", (String.to_json f)));
           Aws.Util.option_map v.content_length
             (fun f -> ("Content-Length", (Long.to_json f)));
           Aws.Util.option_map v.content_language
             (fun f ->
                ("x-amz-fwd-header-Content-Language", (String.to_json f)));
           Aws.Util.option_map v.content_encoding
             (fun f ->
                ("x-amz-fwd-header-Content-Encoding", (String.to_json f)));
           Aws.Util.option_map v.content_disposition
             (fun f ->
                ("x-amz-fwd-header-Content-Disposition", (String.to_json f)));
           Aws.Util.option_map v.cache_control
             (fun f -> ("x-amz-fwd-header-Cache-Control", (String.to_json f)));
           Aws.Util.option_map v.accept_ranges
             (fun f -> ("x-amz-fwd-header-accept-ranges", (String.to_json f)));
           Aws.Util.option_map v.error_message
             (fun f -> ("x-amz-fwd-error-message", (String.to_json f)));
           Aws.Util.option_map v.error_code
             (fun f -> ("x-amz-fwd-error-code", (String.to_json f)));
           Aws.Util.option_map v.status_code
             (fun f -> ("x-amz-fwd-status", (Integer.to_json f)));
           Aws.Util.option_map v.body (fun f -> ("Body", (Blob.to_json f)));
           Some ("x-amz-request-token", (String.to_json v.request_token));
           Some ("x-amz-request-route", (String.to_json v.request_route))])
    let of_json j =
      {
        request_route =
          (String.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "x-amz-request-route")));
        request_token =
          (String.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "x-amz-request-token")));
        body = (Aws.Util.option_map (Aws.Json.lookup j "Body") Blob.of_json);
        status_code =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-fwd-status")
             Integer.of_json);
        error_code =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-fwd-error-code")
             String.of_json);
        error_message =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-fwd-error-message")
             String.of_json);
        accept_ranges =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-accept-ranges")
             String.of_json);
        cache_control =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-Cache-Control")
             String.of_json);
        content_disposition =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-Content-Disposition")
             String.of_json);
        content_encoding =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-Content-Encoding")
             String.of_json);
        content_language =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-Content-Language")
             String.of_json);
        content_length =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-Length")
             Long.of_json);
        content_range =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-Content-Range")
             String.of_json);
        content_type =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-Content-Type")
             String.of_json);
        checksum_c_r_c32 =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-checksum-crc32")
             String.of_json);
        checksum_c_r_c32_c =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-checksum-crc32c")
             String.of_json);
        checksum_s_h_a1 =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-checksum-sha1")
             String.of_json);
        checksum_s_h_a256 =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-checksum-sha256")
             String.of_json);
        delete_marker =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-delete-marker")
             Boolean.of_json);
        e_tag =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-fwd-header-ETag")
             String.of_json);
        expires =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-fwd-header-Expires")
             DateTime.of_json);
        expiration =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-expiration")
             String.of_json);
        last_modified =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-Last-Modified")
             DateTime.of_json);
        missing_meta =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-missing-meta")
             Integer.of_json);
        metadata =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-meta-")
             Metadata.of_json);
        object_lock_mode =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-object-lock-mode")
             ObjectLockMode.of_json);
        object_lock_legal_hold_status =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-fwd-header-x-amz-object-lock-legal-hold")
             ObjectLockLegalHoldStatus.of_json);
        object_lock_retain_until_date =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-fwd-header-x-amz-object-lock-retain-until-date")
             DateTime.of_json);
        parts_count =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-mp-parts-count")
             Integer.of_json);
        replication_status =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-replication-status")
             ReplicationStatus.of_json);
        request_charged =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-request-charged")
             RequestCharged.of_json);
        restore =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-restore")
             String.of_json);
        server_side_encryption =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-fwd-header-x-amz-server-side-encryption")
             ServerSideEncryption.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-fwd-header-x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-fwd-header-x-amz-server-side-encryption-aws-kms-key-id")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-fwd-header-x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        storage_class =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-storage-class")
             StorageClass.of_json);
        tag_count =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-tagging-count")
             Integer.of_json);
        version_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-fwd-header-x-amz-version-id")
             String.of_json);
        bucket_key_enabled =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-fwd-header-x-amz-server-side-encryption-bucket-key-enabled")
             Boolean.of_json)
      }
  end
module DeleteMarkerEntry =
  struct
    type t =
      {
      owner: Owner.t option ;
      key: String.t option ;
      version_id: String.t option ;
      is_latest: Boolean.t option ;
      last_modified: DateTime.t option }
    let make ?owner  ?key  ?version_id  ?is_latest  ?last_modified  () =
      { owner; key; version_id; is_latest; last_modified }
    let parse xml =
      Some
        {
          owner =
            (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) Owner.parse);
          key =
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "VersionId" xml)
               String.parse);
          is_latest =
            (Aws.Util.option_bind (Aws.Xml.member "IsLatest" xml)
               Boolean.parse);
          last_modified =
            (Aws.Util.option_bind (Aws.Xml.member "LastModified" xml)
               DateTime.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.last_modified
              (fun f ->
                 Aws.Query.Pair ("LastModified", (DateTime.to_query f)));
           Aws.Util.option_map v.is_latest
             (fun f -> Aws.Query.Pair ("IsLatest", (Boolean.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("VersionId", (String.to_query f)));
           Aws.Util.option_map v.key
             (fun f -> Aws.Query.Pair ("Key", (String.to_query f)));
           Aws.Util.option_map v.owner
             (fun f -> Aws.Query.Pair ("Owner", (Owner.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.last_modified
              (fun f -> ("LastModified", (DateTime.to_json f)));
           Aws.Util.option_map v.is_latest
             (fun f -> ("IsLatest", (Boolean.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("VersionId", (String.to_json f)));
           Aws.Util.option_map v.key (fun f -> ("Key", (String.to_json f)));
           Aws.Util.option_map v.owner
             (fun f -> ("Owner", (Owner.to_json f)))])
    let of_json j =
      {
        owner =
          (Aws.Util.option_map (Aws.Json.lookup j "Owner") Owner.of_json);
        key = (Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "VersionId") String.of_json);
        is_latest =
          (Aws.Util.option_map (Aws.Json.lookup j "IsLatest") Boolean.of_json);
        last_modified =
          (Aws.Util.option_map (Aws.Json.lookup j "LastModified")
             DateTime.of_json)
      }
  end
module PutBucketRequestPaymentRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      request_payment_configuration: RequestPaymentConfiguration.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?content_m_d5  ?checksum_algorithm 
      ~request_payment_configuration  ?expected_bucket_owner  () =
      {
        bucket;
        content_m_d5;
        checksum_algorithm;
        request_payment_configuration;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          request_payment_configuration =
            (Aws.Xml.required "RequestPaymentConfiguration"
               (Aws.Util.option_bind
                  (Aws.Xml.member "RequestPaymentConfiguration" xml)
                  RequestPaymentConfiguration.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("RequestPaymentConfiguration",
                  (RequestPaymentConfiguration.to_query
                     v.request_payment_configuration)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some
             ("RequestPaymentConfiguration",
               (RequestPaymentConfiguration.to_json
                  v.request_payment_configuration));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        request_payment_configuration =
          (RequestPaymentConfiguration.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "RequestPaymentConfiguration")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module VersioningConfiguration =
  struct
    type t =
      {
      m_f_a_delete: MFADelete.t option ;
      status: BucketVersioningStatus.t option }
    let make ?m_f_a_delete  ?status  () = { m_f_a_delete; status }
    let parse xml =
      Some
        {
          m_f_a_delete =
            (Aws.Util.option_bind (Aws.Xml.member "MfaDelete" xml)
               MFADelete.parse);
          status =
            (Aws.Util.option_bind (Aws.Xml.member "Status" xml)
               BucketVersioningStatus.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.status
              (fun f ->
                 Aws.Query.Pair
                   ("Status", (BucketVersioningStatus.to_query f)));
           Aws.Util.option_map v.m_f_a_delete
             (fun f -> Aws.Query.Pair ("MfaDelete", (MFADelete.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.status
              (fun f -> ("Status", (BucketVersioningStatus.to_json f)));
           Aws.Util.option_map v.m_f_a_delete
             (fun f -> ("MfaDelete", (MFADelete.to_json f)))])
    let of_json j =
      {
        m_f_a_delete =
          (Aws.Util.option_map (Aws.Json.lookup j "MfaDelete")
             MFADelete.of_json);
        status =
          (Aws.Util.option_map (Aws.Json.lookup j "Status")
             BucketVersioningStatus.of_json)
      }
  end
module PutBucketVersioningRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      m_f_a: String.t option ;
      versioning_configuration: VersioningConfiguration.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?content_m_d5  ?checksum_algorithm  ?m_f_a 
      ~versioning_configuration  ?expected_bucket_owner  () =
      {
        bucket;
        content_m_d5;
        checksum_algorithm;
        m_f_a;
        versioning_configuration;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          m_f_a =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-mfa" xml)
               String.parse);
          versioning_configuration =
            (Aws.Xml.required "VersioningConfiguration"
               (Aws.Util.option_bind
                  (Aws.Xml.member "VersioningConfiguration" xml)
                  VersioningConfiguration.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("VersioningConfiguration",
                  (VersioningConfiguration.to_query
                     v.versioning_configuration)));
           Aws.Util.option_map v.m_f_a
             (fun f -> Aws.Query.Pair ("x-amz-mfa", (String.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some
             ("VersioningConfiguration",
               (VersioningConfiguration.to_json v.versioning_configuration));
           Aws.Util.option_map v.m_f_a
             (fun f -> ("x-amz-mfa", (String.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        m_f_a =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-mfa") String.of_json);
        versioning_configuration =
          (VersioningConfiguration.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "VersioningConfiguration")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module ObjectNotInActiveTierError =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])
    let to_json v = `Assoc (Aws.Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteMarkers =
  struct
    type t = DeleteMarkerEntry.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map DeleteMarkerEntry.parse (Aws.Xml.members "member" xml))
    let to_query v = Aws.Query.to_query_list DeleteMarkerEntry.to_query v
    let to_json v = `List (List.map DeleteMarkerEntry.to_json v)
    let of_json j = Aws.Json.to_list DeleteMarkerEntry.of_json j
  end
module ListObjectVersionsOutput =
  struct
    type t =
      {
      is_truncated: Boolean.t option ;
      key_marker: String.t option ;
      version_id_marker: String.t option ;
      next_key_marker: String.t option ;
      next_version_id_marker: String.t option ;
      versions: ObjectVersionList.t ;
      delete_markers: DeleteMarkers.t ;
      name: String.t option ;
      prefix: String.t option ;
      delimiter: String.t option ;
      max_keys: Integer.t option ;
      common_prefixes: CommonPrefixList.t ;
      encoding_type: EncodingType.t option ;
      request_charged: RequestCharged.t option }
    let make ?is_truncated  ?key_marker  ?version_id_marker  ?next_key_marker
       ?next_version_id_marker  ?(versions= [])  ?(delete_markers= [])  ?name
       ?prefix  ?delimiter  ?max_keys  ?(common_prefixes= [])  ?encoding_type
       ?request_charged  () =
      {
        is_truncated;
        key_marker;
        version_id_marker;
        next_key_marker;
        next_version_id_marker;
        versions;
        delete_markers;
        name;
        prefix;
        delimiter;
        max_keys;
        common_prefixes;
        encoding_type;
        request_charged
      }
    let parse xml =
      Some
        {
          is_truncated =
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml)
               Boolean.parse);
          key_marker =
            (Aws.Util.option_bind (Aws.Xml.member "KeyMarker" xml)
               String.parse);
          version_id_marker =
            (Aws.Util.option_bind (Aws.Xml.member "VersionIdMarker" xml)
               String.parse);
          next_key_marker =
            (Aws.Util.option_bind (Aws.Xml.member "NextKeyMarker" xml)
               String.parse);
          next_version_id_marker =
            (Aws.Util.option_bind (Aws.Xml.member "NextVersionIdMarker" xml)
               String.parse);
          versions = (Aws.Util.of_option [] (ObjectVersionList.parse xml));
          delete_markers = (Aws.Util.of_option [] (DeleteMarkers.parse xml));
          name =
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse);
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          delimiter =
            (Aws.Util.option_bind (Aws.Xml.member "Delimiter" xml)
               String.parse);
          max_keys =
            (Aws.Util.option_bind (Aws.Xml.member "MaxKeys" xml)
               Integer.parse);
          common_prefixes =
            (Aws.Util.of_option [] (CommonPrefixList.parse xml));
          encoding_type =
            (Aws.Util.option_bind (Aws.Xml.member "EncodingType" xml)
               EncodingType.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.encoding_type
             (fun f ->
                Aws.Query.Pair ("EncodingType", (EncodingType.to_query f)));
           Some
             (Aws.Query.Pair
                ("CommonPrefixes.member",
                  (CommonPrefixList.to_query v.common_prefixes)));
           Aws.Util.option_map v.max_keys
             (fun f -> Aws.Query.Pair ("MaxKeys", (Integer.to_query f)));
           Aws.Util.option_map v.delimiter
             (fun f -> Aws.Query.Pair ("Delimiter", (String.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)));
           Aws.Util.option_map v.name
             (fun f -> Aws.Query.Pair ("Name", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("DeleteMarker", (DeleteMarkers.to_query v.delete_markers)));
           Some
             (Aws.Query.Pair
                ("Version", (ObjectVersionList.to_query v.versions)));
           Aws.Util.option_map v.next_version_id_marker
             (fun f ->
                Aws.Query.Pair ("NextVersionIdMarker", (String.to_query f)));
           Aws.Util.option_map v.next_key_marker
             (fun f -> Aws.Query.Pair ("NextKeyMarker", (String.to_query f)));
           Aws.Util.option_map v.version_id_marker
             (fun f ->
                Aws.Query.Pair ("VersionIdMarker", (String.to_query f)));
           Aws.Util.option_map v.key_marker
             (fun f -> Aws.Query.Pair ("KeyMarker", (String.to_query f)));
           Aws.Util.option_map v.is_truncated
             (fun f -> Aws.Query.Pair ("IsTruncated", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.encoding_type
             (fun f -> ("EncodingType", (EncodingType.to_json f)));
           Some
             ("CommonPrefixes", (CommonPrefixList.to_json v.common_prefixes));
           Aws.Util.option_map v.max_keys
             (fun f -> ("MaxKeys", (Integer.to_json f)));
           Aws.Util.option_map v.delimiter
             (fun f -> ("Delimiter", (String.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)));
           Aws.Util.option_map v.name (fun f -> ("Name", (String.to_json f)));
           Some ("DeleteMarker", (DeleteMarkers.to_json v.delete_markers));
           Some ("Version", (ObjectVersionList.to_json v.versions));
           Aws.Util.option_map v.next_version_id_marker
             (fun f -> ("NextVersionIdMarker", (String.to_json f)));
           Aws.Util.option_map v.next_key_marker
             (fun f -> ("NextKeyMarker", (String.to_json f)));
           Aws.Util.option_map v.version_id_marker
             (fun f -> ("VersionIdMarker", (String.to_json f)));
           Aws.Util.option_map v.key_marker
             (fun f -> ("KeyMarker", (String.to_json f)));
           Aws.Util.option_map v.is_truncated
             (fun f -> ("IsTruncated", (Boolean.to_json f)))])
    let of_json j =
      {
        is_truncated =
          (Aws.Util.option_map (Aws.Json.lookup j "IsTruncated")
             Boolean.of_json);
        key_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "KeyMarker") String.of_json);
        version_id_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "VersionIdMarker")
             String.of_json);
        next_key_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "NextKeyMarker")
             String.of_json);
        next_version_id_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "NextVersionIdMarker")
             String.of_json);
        versions =
          (ObjectVersionList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Version")));
        delete_markers =
          (DeleteMarkers.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "DeleteMarker")));
        name =
          (Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json);
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        delimiter =
          (Aws.Util.option_map (Aws.Json.lookup j "Delimiter") String.of_json);
        max_keys =
          (Aws.Util.option_map (Aws.Json.lookup j "MaxKeys") Integer.of_json);
        common_prefixes =
          (CommonPrefixList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "CommonPrefixes")));
        encoding_type =
          (Aws.Util.option_map (Aws.Json.lookup j "EncodingType")
             EncodingType.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module RestoreRequestType =
  struct
    type t =
      | SELECT 
    let str_to_t = [("SELECT", SELECT)]
    let t_to_str = [(SELECT, "SELECT")]
    let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
    let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Aws.Util.option_bind (String.parse xml)
        (fun s -> Aws.Util.list_find str_to_t s)
    let to_query v =
      Aws.Query.Value
        (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
    let of_json j =
      Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
  end
module SelectParameters =
  struct
    type t =
      {
      input_serialization: InputSerialization.t ;
      expression_type: ExpressionType.t ;
      expression: String.t ;
      output_serialization: OutputSerialization.t }
    let make ~input_serialization  ~expression_type  ~expression 
      ~output_serialization  () =
      {
        input_serialization;
        expression_type;
        expression;
        output_serialization
      }
    let parse xml =
      Some
        {
          input_serialization =
            (Aws.Xml.required "InputSerialization"
               (Aws.Util.option_bind
                  (Aws.Xml.member "InputSerialization" xml)
                  InputSerialization.parse));
          expression_type =
            (Aws.Xml.required "ExpressionType"
               (Aws.Util.option_bind (Aws.Xml.member "ExpressionType" xml)
                  ExpressionType.parse));
          expression =
            (Aws.Xml.required "Expression"
               (Aws.Util.option_bind (Aws.Xml.member "Expression" xml)
                  String.parse));
          output_serialization =
            (Aws.Xml.required "OutputSerialization"
               (Aws.Util.option_bind
                  (Aws.Xml.member "OutputSerialization" xml)
                  OutputSerialization.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("OutputSerialization",
                   (OutputSerialization.to_query v.output_serialization)));
           Some
             (Aws.Query.Pair ("Expression", (String.to_query v.expression)));
           Some
             (Aws.Query.Pair
                ("ExpressionType",
                  (ExpressionType.to_query v.expression_type)));
           Some
             (Aws.Query.Pair
                ("InputSerialization",
                  (InputSerialization.to_query v.input_serialization)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("OutputSerialization",
                (OutputSerialization.to_json v.output_serialization));
           Some ("Expression", (String.to_json v.expression));
           Some
             ("ExpressionType", (ExpressionType.to_json v.expression_type));
           Some
             ("InputSerialization",
               (InputSerialization.to_json v.input_serialization))])
    let of_json j =
      {
        input_serialization =
          (InputSerialization.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "InputSerialization")));
        expression_type =
          (ExpressionType.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "ExpressionType")));
        expression =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Expression")));
        output_serialization =
          (OutputSerialization.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "OutputSerialization")))
      }
  end
module RestoreRequest =
  struct
    type t =
      {
      days: Integer.t option ;
      glacier_job_parameters: GlacierJobParameters.t option ;
      type_: RestoreRequestType.t option ;
      tier: Tier.t option ;
      description: String.t option ;
      select_parameters: SelectParameters.t option ;
      output_location: OutputLocation.t option }
    let make ?days  ?glacier_job_parameters  ?type_  ?tier  ?description 
      ?select_parameters  ?output_location  () =
      {
        days;
        glacier_job_parameters;
        type_;
        tier;
        description;
        select_parameters;
        output_location
      }
    let parse xml =
      Some
        {
          days =
            (Aws.Util.option_bind (Aws.Xml.member "Days" xml) Integer.parse);
          glacier_job_parameters =
            (Aws.Util.option_bind (Aws.Xml.member "GlacierJobParameters" xml)
               GlacierJobParameters.parse);
          type_ =
            (Aws.Util.option_bind (Aws.Xml.member "Type" xml)
               RestoreRequestType.parse);
          tier =
            (Aws.Util.option_bind (Aws.Xml.member "Tier" xml) Tier.parse);
          description =
            (Aws.Util.option_bind (Aws.Xml.member "Description" xml)
               String.parse);
          select_parameters =
            (Aws.Util.option_bind (Aws.Xml.member "SelectParameters" xml)
               SelectParameters.parse);
          output_location =
            (Aws.Util.option_bind (Aws.Xml.member "OutputLocation" xml)
               OutputLocation.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.output_location
              (fun f ->
                 Aws.Query.Pair
                   ("OutputLocation", (OutputLocation.to_query f)));
           Aws.Util.option_map v.select_parameters
             (fun f ->
                Aws.Query.Pair
                  ("SelectParameters", (SelectParameters.to_query f)));
           Aws.Util.option_map v.description
             (fun f -> Aws.Query.Pair ("Description", (String.to_query f)));
           Aws.Util.option_map v.tier
             (fun f -> Aws.Query.Pair ("Tier", (Tier.to_query f)));
           Aws.Util.option_map v.type_
             (fun f ->
                Aws.Query.Pair ("Type", (RestoreRequestType.to_query f)));
           Aws.Util.option_map v.glacier_job_parameters
             (fun f ->
                Aws.Query.Pair
                  ("GlacierJobParameters", (GlacierJobParameters.to_query f)));
           Aws.Util.option_map v.days
             (fun f -> Aws.Query.Pair ("Days", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.output_location
              (fun f -> ("OutputLocation", (OutputLocation.to_json f)));
           Aws.Util.option_map v.select_parameters
             (fun f -> ("SelectParameters", (SelectParameters.to_json f)));
           Aws.Util.option_map v.description
             (fun f -> ("Description", (String.to_json f)));
           Aws.Util.option_map v.tier (fun f -> ("Tier", (Tier.to_json f)));
           Aws.Util.option_map v.type_
             (fun f -> ("Type", (RestoreRequestType.to_json f)));
           Aws.Util.option_map v.glacier_job_parameters
             (fun f ->
                ("GlacierJobParameters", (GlacierJobParameters.to_json f)));
           Aws.Util.option_map v.days
             (fun f -> ("Days", (Integer.to_json f)))])
    let of_json j =
      {
        days =
          (Aws.Util.option_map (Aws.Json.lookup j "Days") Integer.of_json);
        glacier_job_parameters =
          (Aws.Util.option_map (Aws.Json.lookup j "GlacierJobParameters")
             GlacierJobParameters.of_json);
        type_ =
          (Aws.Util.option_map (Aws.Json.lookup j "Type")
             RestoreRequestType.of_json);
        tier = (Aws.Util.option_map (Aws.Json.lookup j "Tier") Tier.of_json);
        description =
          (Aws.Util.option_map (Aws.Json.lookup j "Description")
             String.of_json);
        select_parameters =
          (Aws.Util.option_map (Aws.Json.lookup j "SelectParameters")
             SelectParameters.of_json);
        output_location =
          (Aws.Util.option_map (Aws.Json.lookup j "OutputLocation")
             OutputLocation.of_json)
      }
  end
module RestoreObjectRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      version_id: String.t option ;
      restore_request: RestoreRequest.t option ;
      request_payer: RequestPayer.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~key  ?version_id  ?restore_request  ?request_payer 
      ?checksum_algorithm  ?expected_bucket_owner  () =
      {
        bucket;
        key;
        version_id;
        restore_request;
        request_payer;
        checksum_algorithm;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          restore_request =
            (Aws.Util.option_bind (Aws.Xml.member "RestoreRequest" xml)
               RestoreRequest.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.restore_request
             (fun f ->
                Aws.Query.Pair
                  ("RestoreRequest", (RestoreRequest.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.restore_request
             (fun f -> ("RestoreRequest", (RestoreRequest.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        restore_request =
          (Aws.Util.option_map (Aws.Json.lookup j "RestoreRequest")
             RestoreRequest.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetBucketPolicyOutput =
  struct
    type t = {
      policy: String.t option }
    let make ?policy  () = { policy }
    let parse xml =
      Some
        {
          policy =
            (Aws.Util.option_bind (Aws.Xml.member "Policy" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.policy
              (fun f -> Aws.Query.Pair ("Policy", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.policy
              (fun f -> ("Policy", (String.to_json f)))])
    let of_json j =
      {
        policy =
          (Aws.Util.option_map (Aws.Json.lookup j "Policy") String.of_json)
      }
  end
module GetBucketLocationRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module PutBucketMetricsConfigurationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      id: String.t ;
      metrics_configuration: MetricsConfiguration.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~id  ~metrics_configuration  ?expected_bucket_owner  ()
      = { bucket; id; metrics_configuration; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          id =
            (Aws.Xml.required "id"
               (Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse));
          metrics_configuration =
            (Aws.Xml.required "MetricsConfiguration"
               (Aws.Util.option_bind
                  (Aws.Xml.member "MetricsConfiguration" xml)
                  MetricsConfiguration.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("MetricsConfiguration",
                  (MetricsConfiguration.to_query v.metrics_configuration)));
           Some (Aws.Query.Pair ("id", (String.to_query v.id)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some
             ("MetricsConfiguration",
               (MetricsConfiguration.to_json v.metrics_configuration));
           Some ("id", (String.to_json v.id));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        id =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "id")));
        metrics_configuration =
          (MetricsConfiguration.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "MetricsConfiguration")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetBucketLifecycleConfigurationOutput =
  struct
    type t = {
      rules: LifecycleRules.t }
    let make ?(rules= [])  () = { rules }
    let parse xml =
      Some { rules = (Aws.Util.of_option [] (LifecycleRules.parse xml)) }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Rule", (LifecycleRules.to_query v.rules)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("Rule", (LifecycleRules.to_json v.rules))])
    let of_json j =
      {
        rules =
          (LifecycleRules.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Rule")))
      }
  end
module ObjectAlreadyInActiveTierError =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])
    let to_json v = `Assoc (Aws.Util.list_filter_opt [])
    let of_json j = ()
  end
module NoSuchBucket =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])
    let to_json v = `Assoc (Aws.Util.list_filter_opt [])
    let of_json j = ()
  end
module PutBucketOwnershipControlsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      expected_bucket_owner: String.t option ;
      ownership_controls: OwnershipControls.t }
    let make ~bucket  ?content_m_d5  ?expected_bucket_owner 
      ~ownership_controls  () =
      { bucket; content_m_d5; expected_bucket_owner; ownership_controls }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          ownership_controls =
            (Aws.Xml.required "OwnershipControls"
               (Aws.Util.option_bind (Aws.Xml.member "OwnershipControls" xml)
                  OwnershipControls.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("OwnershipControls",
                   (OwnershipControls.to_query v.ownership_controls)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("OwnershipControls",
                (OwnershipControls.to_json v.ownership_controls));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        ownership_controls =
          (OwnershipControls.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "OwnershipControls")))
      }
  end
module GetObjectTaggingOutput =
  struct
    type t = {
      version_id: String.t option ;
      tag_set: TagSet.t }
    let make ?version_id  ~tag_set  () = { version_id; tag_set }
    let parse xml =
      Some
        {
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-version-id" xml)
               String.parse);
          tag_set =
            (Aws.Xml.required "TagSet"
               (Aws.Util.option_bind (Aws.Xml.member "TagSet" xml)
                  TagSet.parse))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair ("TagSet.member", (TagSet.to_query v.tag_set)));
           Aws.Util.option_map v.version_id
             (fun f ->
                Aws.Query.Pair ("x-amz-version-id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some ("TagSet", (TagSet.to_json v.tag_set));
           Aws.Util.option_map v.version_id
             (fun f -> ("x-amz-version-id", (String.to_json f)))])
    let of_json j =
      {
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-version-id")
             String.of_json);
        tag_set =
          (TagSet.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSet")))
      }
  end
module GetObjectLockConfigurationRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module AnalyticsConfigurationList =
  struct
    type t = AnalyticsConfiguration.t list
    let make elems () = elems
    let parse xml =
      Aws.Util.option_all
        (List.map AnalyticsConfiguration.parse (Aws.Xml.members "member" xml))
    let to_query v =
      Aws.Query.to_query_list AnalyticsConfiguration.to_query v
    let to_json v = `List (List.map AnalyticsConfiguration.to_json v)
    let of_json j = Aws.Json.to_list AnalyticsConfiguration.of_json j
  end
module ListBucketAnalyticsConfigurationsOutput =
  struct
    type t =
      {
      is_truncated: Boolean.t option ;
      continuation_token: String.t option ;
      next_continuation_token: String.t option ;
      analytics_configuration_list: AnalyticsConfigurationList.t }
    let make ?is_truncated  ?continuation_token  ?next_continuation_token 
      ?(analytics_configuration_list= [])  () =
      {
        is_truncated;
        continuation_token;
        next_continuation_token;
        analytics_configuration_list
      }
    let parse xml =
      Some
        {
          is_truncated =
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml)
               Boolean.parse);
          continuation_token =
            (Aws.Util.option_bind (Aws.Xml.member "ContinuationToken" xml)
               String.parse);
          next_continuation_token =
            (Aws.Util.option_bind
               (Aws.Xml.member "NextContinuationToken" xml) String.parse);
          analytics_configuration_list =
            (Aws.Util.of_option [] (AnalyticsConfigurationList.parse xml))
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some
              (Aws.Query.Pair
                 ("AnalyticsConfiguration",
                   (AnalyticsConfigurationList.to_query
                      v.analytics_configuration_list)));
           Aws.Util.option_map v.next_continuation_token
             (fun f ->
                Aws.Query.Pair ("NextContinuationToken", (String.to_query f)));
           Aws.Util.option_map v.continuation_token
             (fun f ->
                Aws.Query.Pair ("ContinuationToken", (String.to_query f)));
           Aws.Util.option_map v.is_truncated
             (fun f -> Aws.Query.Pair ("IsTruncated", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Some
              ("AnalyticsConfiguration",
                (AnalyticsConfigurationList.to_json
                   v.analytics_configuration_list));
           Aws.Util.option_map v.next_continuation_token
             (fun f -> ("NextContinuationToken", (String.to_json f)));
           Aws.Util.option_map v.continuation_token
             (fun f -> ("ContinuationToken", (String.to_json f)));
           Aws.Util.option_map v.is_truncated
             (fun f -> ("IsTruncated", (Boolean.to_json f)))])
    let of_json j =
      {
        is_truncated =
          (Aws.Util.option_map (Aws.Json.lookup j "IsTruncated")
             Boolean.of_json);
        continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "ContinuationToken")
             String.of_json);
        next_continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "NextContinuationToken")
             String.of_json);
        analytics_configuration_list =
          (AnalyticsConfigurationList.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "AnalyticsConfiguration")))
      }
  end
module GetBucketVersioningRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetBucketPolicyRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module PutObjectTaggingOutput =
  struct
    type t = {
      version_id: String.t option }
    let make ?version_id  () = { version_id }
    let parse xml =
      Some
        {
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-version-id" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.version_id
              (fun f ->
                 Aws.Query.Pair ("x-amz-version-id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.version_id
              (fun f -> ("x-amz-version-id", (String.to_json f)))])
    let of_json j =
      {
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-version-id")
             String.of_json)
      }
  end
module GetBucketLocationOutput =
  struct
    type t = {
      location_constraint: BucketLocationConstraint.t option }
    let make ?location_constraint  () = { location_constraint }
    let parse xml =
      Some
        {
          location_constraint =
            (Aws.Util.option_bind (Aws.Xml.member "LocationConstraint" xml)
               BucketLocationConstraint.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.location_constraint
              (fun f ->
                 Aws.Query.Pair
                   ("LocationConstraint",
                     (BucketLocationConstraint.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.location_constraint
              (fun f ->
                 ("LocationConstraint", (BucketLocationConstraint.to_json f)))])
    let of_json j =
      {
        location_constraint =
          (Aws.Util.option_map (Aws.Json.lookup j "LocationConstraint")
             BucketLocationConstraint.of_json)
      }
  end
module AbortMultipartUploadRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      upload_id: String.t ;
      request_payer: RequestPayer.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~key  ~upload_id  ?request_payer 
      ?expected_bucket_owner  () =
      { bucket; key; upload_id; request_payer; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          upload_id =
            (Aws.Xml.required "uploadId"
               (Aws.Util.option_bind (Aws.Xml.member "uploadId" xml)
                  String.parse));
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Some (Aws.Query.Pair ("uploadId", (String.to_query v.upload_id)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Some ("uploadId", (String.to_json v.upload_id));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        upload_id =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "uploadId")));
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetBucketTaggingRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module DeleteObjectRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      m_f_a: String.t option ;
      version_id: String.t option ;
      request_payer: RequestPayer.t option ;
      bypass_governance_retention: Boolean.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~key  ?m_f_a  ?version_id  ?request_payer 
      ?bypass_governance_retention  ?expected_bucket_owner  () =
      {
        bucket;
        key;
        m_f_a;
        version_id;
        request_payer;
        bypass_governance_retention;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          m_f_a =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-mfa" xml)
               String.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          bypass_governance_retention =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-bypass-governance-retention" xml)
               Boolean.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.bypass_governance_retention
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-bypass-governance-retention", (Boolean.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Aws.Util.option_map v.m_f_a
             (fun f -> Aws.Query.Pair ("x-amz-mfa", (String.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.bypass_governance_retention
             (fun f ->
                ("x-amz-bypass-governance-retention", (Boolean.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Aws.Util.option_map v.m_f_a
             (fun f -> ("x-amz-mfa", (String.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        m_f_a =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-mfa") String.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        bypass_governance_retention =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-bypass-governance-retention")
             Boolean.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module HeadBucketRequest =
  struct
    type t = {
      bucket: String.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?expected_bucket_owner  () =
      { bucket; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module ListPartsRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      max_parts: Integer.t option ;
      part_number_marker: Integer.t option ;
      upload_id: String.t ;
      request_payer: RequestPayer.t option ;
      expected_bucket_owner: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option }
    let make ~bucket  ~key  ?max_parts  ?part_number_marker  ~upload_id 
      ?request_payer  ?expected_bucket_owner  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key  ?s_s_e_customer_key_m_d5  () =
      {
        bucket;
        key;
        max_parts;
        part_number_marker;
        upload_id;
        request_payer;
        expected_bucket_owner;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          max_parts =
            (Aws.Util.option_bind (Aws.Xml.member "max-parts" xml)
               Integer.parse);
          part_number_marker =
            (Aws.Util.option_bind (Aws.Xml.member "part-number-marker" xml)
               Integer.parse);
          upload_id =
            (Aws.Xml.required "uploadId"
               (Aws.Util.option_bind (Aws.Xml.member "uploadId" xml)
                  String.parse));
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-customer-key"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.s_s_e_customer_key_m_d5
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-server-side-encryption-customer-key-MD5",
                     (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Some (Aws.Query.Pair ("uploadId", (String.to_query v.upload_id)));
           Aws.Util.option_map v.part_number_marker
             (fun f ->
                Aws.Query.Pair ("part-number-marker", (Integer.to_query f)));
           Aws.Util.option_map v.max_parts
             (fun f -> Aws.Query.Pair ("max-parts", (Integer.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.s_s_e_customer_key_m_d5
              (fun f ->
                 ("x-amz-server-side-encryption-customer-key-MD5",
                   (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                ("x-amz-server-side-encryption-customer-key",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Some ("uploadId", (String.to_json v.upload_id));
           Aws.Util.option_map v.part_number_marker
             (fun f -> ("part-number-marker", (Integer.to_json f)));
           Aws.Util.option_map v.max_parts
             (fun f -> ("max-parts", (Integer.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        max_parts =
          (Aws.Util.option_map (Aws.Json.lookup j "max-parts")
             Integer.of_json);
        part_number_marker =
          (Aws.Util.option_map (Aws.Json.lookup j "part-number-marker")
             Integer.of_json);
        upload_id =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "uploadId")));
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-customer-key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json)
      }
  end
module GetBucketOwnershipControlsOutput =
  struct
    type t = {
      ownership_controls: OwnershipControls.t option }
    let make ?ownership_controls  () = { ownership_controls }
    let parse xml =
      Some
        {
          ownership_controls =
            (Aws.Util.option_bind (Aws.Xml.member "OwnershipControls" xml)
               OwnershipControls.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.ownership_controls
              (fun f ->
                 Aws.Query.Pair
                   ("OwnershipControls", (OwnershipControls.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.ownership_controls
              (fun f -> ("OwnershipControls", (OwnershipControls.to_json f)))])
    let of_json j =
      {
        ownership_controls =
          (Aws.Util.option_map (Aws.Json.lookup j "OwnershipControls")
             OwnershipControls.of_json)
      }
  end
module DeleteObjectTaggingOutput =
  struct
    type t = {
      version_id: String.t option }
    let make ?version_id  () = { version_id }
    let parse xml =
      Some
        {
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-version-id" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.version_id
              (fun f ->
                 Aws.Query.Pair ("x-amz-version-id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.version_id
              (fun f -> ("x-amz-version-id", (String.to_json f)))])
    let of_json j =
      {
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-version-id")
             String.of_json)
      }
  end
module PutBucketLifecycleRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      lifecycle_configuration: LifecycleConfiguration.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?content_m_d5  ?checksum_algorithm 
      ?lifecycle_configuration  ?expected_bucket_owner  () =
      {
        bucket;
        content_m_d5;
        checksum_algorithm;
        lifecycle_configuration;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          lifecycle_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "LifecycleConfiguration" xml)
               LifecycleConfiguration.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.lifecycle_configuration
             (fun f ->
                Aws.Query.Pair
                  ("LifecycleConfiguration",
                    (LifecycleConfiguration.to_query f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.lifecycle_configuration
             (fun f ->
                ("LifecycleConfiguration",
                  (LifecycleConfiguration.to_json f)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        lifecycle_configuration =
          (Aws.Util.option_map (Aws.Json.lookup j "LifecycleConfiguration")
             LifecycleConfiguration.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetBucketLifecycleOutput =
  struct
    type t = {
      rules: Rules.t }
    let make ?(rules= [])  () = { rules }
    let parse xml =
      Some { rules = (Aws.Util.of_option [] (Rules.parse xml)) }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Some (Aws.Query.Pair ("Rule", (Rules.to_query v.rules)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt [Some ("Rule", (Rules.to_json v.rules))])
    let of_json j =
      {
        rules =
          (Rules.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Rule")))
      }
  end
module PutBucketEncryptionRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      server_side_encryption_configuration:
        ServerSideEncryptionConfiguration.t ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?content_m_d5  ?checksum_algorithm 
      ~server_side_encryption_configuration  ?expected_bucket_owner  () =
      {
        bucket;
        content_m_d5;
        checksum_algorithm;
        server_side_encryption_configuration;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          server_side_encryption_configuration =
            (Aws.Xml.required "ServerSideEncryptionConfiguration"
               (Aws.Util.option_bind
                  (Aws.Xml.member "ServerSideEncryptionConfiguration" xml)
                  ServerSideEncryptionConfiguration.parse));
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("ServerSideEncryptionConfiguration",
                  (ServerSideEncryptionConfiguration.to_query
                     v.server_side_encryption_configuration)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Some
             ("ServerSideEncryptionConfiguration",
               (ServerSideEncryptionConfiguration.to_json
                  v.server_side_encryption_configuration));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        server_side_encryption_configuration =
          (ServerSideEncryptionConfiguration.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "ServerSideEncryptionConfiguration")));
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module RestoreObjectOutput =
  struct
    type t =
      {
      request_charged: RequestCharged.t option ;
      restore_output_path: String.t option }
    let make ?request_charged  ?restore_output_path  () =
      { request_charged; restore_output_path }
    let parse xml =
      Some
        {
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse);
          restore_output_path =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-restore-output-path" xml) String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.restore_output_path
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-restore-output-path", (String.to_query f)));
           Aws.Util.option_map v.request_charged
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-charged", (RequestCharged.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.restore_output_path
              (fun f -> ("x-amz-restore-output-path", (String.to_json f)));
           Aws.Util.option_map v.request_charged
             (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)))])
    let of_json j =
      {
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json);
        restore_output_path =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-restore-output-path") String.of_json)
      }
  end
module PutBucketReplicationRequest =
  struct
    type t =
      {
      bucket: String.t ;
      content_m_d5: String.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option ;
      replication_configuration: ReplicationConfiguration.t ;
      token: String.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ?content_m_d5  ?checksum_algorithm 
      ~replication_configuration  ?token  ?expected_bucket_owner  () =
      {
        bucket;
        content_m_d5;
        checksum_algorithm;
        replication_configuration;
        token;
        expected_bucket_owner
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          content_m_d5 =
            (Aws.Util.option_bind (Aws.Xml.member "Content-MD5" xml)
               String.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-sdk-checksum-algorithm" xml)
               ChecksumAlgorithm.parse);
          replication_configuration =
            (Aws.Xml.required "ReplicationConfiguration"
               (Aws.Util.option_bind
                  (Aws.Xml.member "ReplicationConfiguration" xml)
                  ReplicationConfiguration.parse));
          token =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-bucket-object-lock-token" xml)
               String.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.token
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-bucket-object-lock-token", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("ReplicationConfiguration",
                  (ReplicationConfiguration.to_query
                     v.replication_configuration)));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-sdk-checksum-algorithm",
                    (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> Aws.Query.Pair ("Content-MD5", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.token
             (fun f -> ("x-amz-bucket-object-lock-token", (String.to_json f)));
           Some
             ("ReplicationConfiguration",
               (ReplicationConfiguration.to_json v.replication_configuration));
           Aws.Util.option_map v.checksum_algorithm
             (fun f ->
                ("x-amz-sdk-checksum-algorithm",
                  (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.content_m_d5
             (fun f -> ("Content-MD5", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        content_m_d5 =
          (Aws.Util.option_map (Aws.Json.lookup j "Content-MD5")
             String.of_json);
        checksum_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-sdk-checksum-algorithm")
             ChecksumAlgorithm.of_json);
        replication_configuration =
          (ReplicationConfiguration.of_json
             (Aws.Util.of_option_exn
                (Aws.Json.lookup j "ReplicationConfiguration")));
        token =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-bucket-object-lock-token")
             String.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetBucketPolicyStatusOutput =
  struct
    type t = {
      policy_status: PolicyStatus.t option }
    let make ?policy_status  () = { policy_status }
    let parse xml =
      Some
        {
          policy_status =
            (Aws.Util.option_bind (Aws.Xml.member "PolicyStatus" xml)
               PolicyStatus.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.policy_status
              (fun f ->
                 Aws.Query.Pair ("PolicyStatus", (PolicyStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.policy_status
              (fun f -> ("PolicyStatus", (PolicyStatus.to_json f)))])
    let of_json j =
      {
        policy_status =
          (Aws.Util.option_map (Aws.Json.lookup j "PolicyStatus")
             PolicyStatus.of_json)
      }
  end
module GetObjectRequest =
  struct
    type t =
      {
      bucket: String.t ;
      if_match: String.t option ;
      if_modified_since: DateTime.t option ;
      if_none_match: String.t option ;
      if_unmodified_since: DateTime.t option ;
      key: String.t ;
      range: String.t option ;
      response_cache_control: String.t option ;
      response_content_disposition: String.t option ;
      response_content_encoding: String.t option ;
      response_content_language: String.t option ;
      response_content_type: String.t option ;
      response_expires: DateTime.t option ;
      version_id: String.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      request_payer: RequestPayer.t option ;
      part_number: Integer.t option ;
      expected_bucket_owner: String.t option ;
      checksum_mode: ChecksumMode.t option }
    let make ~bucket  ?if_match  ?if_modified_since  ?if_none_match 
      ?if_unmodified_since  ~key  ?range  ?response_cache_control 
      ?response_content_disposition  ?response_content_encoding 
      ?response_content_language  ?response_content_type  ?response_expires 
      ?version_id  ?s_s_e_customer_algorithm  ?s_s_e_customer_key 
      ?s_s_e_customer_key_m_d5  ?request_payer  ?part_number 
      ?expected_bucket_owner  ?checksum_mode  () =
      {
        bucket;
        if_match;
        if_modified_since;
        if_none_match;
        if_unmodified_since;
        key;
        range;
        response_cache_control;
        response_content_disposition;
        response_content_encoding;
        response_content_language;
        response_content_type;
        response_expires;
        version_id;
        s_s_e_customer_algorithm;
        s_s_e_customer_key;
        s_s_e_customer_key_m_d5;
        request_payer;
        part_number;
        expected_bucket_owner;
        checksum_mode
      }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          if_match =
            (Aws.Util.option_bind (Aws.Xml.member "If-Match" xml)
               String.parse);
          if_modified_since =
            (Aws.Util.option_bind (Aws.Xml.member "If-Modified-Since" xml)
               DateTime.parse);
          if_none_match =
            (Aws.Util.option_bind (Aws.Xml.member "If-None-Match" xml)
               String.parse);
          if_unmodified_since =
            (Aws.Util.option_bind (Aws.Xml.member "If-Unmodified-Since" xml)
               DateTime.parse);
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          range =
            (Aws.Util.option_bind (Aws.Xml.member "Range" xml) String.parse);
          response_cache_control =
            (Aws.Util.option_bind
               (Aws.Xml.member "response-cache-control" xml) String.parse);
          response_content_disposition =
            (Aws.Util.option_bind
               (Aws.Xml.member "response-content-disposition" xml)
               String.parse);
          response_content_encoding =
            (Aws.Util.option_bind
               (Aws.Xml.member "response-content-encoding" xml) String.parse);
          response_content_language =
            (Aws.Util.option_bind
               (Aws.Xml.member "response-content-language" xml) String.parse);
          response_content_type =
            (Aws.Util.option_bind
               (Aws.Xml.member "response-content-type" xml) String.parse);
          response_expires =
            (Aws.Util.option_bind (Aws.Xml.member "response-expires" xml)
               DateTime.parse);
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-customer-key"
                  xml) String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          part_number =
            (Aws.Util.option_bind (Aws.Xml.member "partNumber" xml)
               Integer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          checksum_mode =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-checksum-mode" xml)
               ChecksumMode.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_mode
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-checksum-mode", (ChecksumMode.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.part_number
             (fun f -> Aws.Query.Pair ("partNumber", (Integer.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Aws.Util.option_map v.response_expires
             (fun f ->
                Aws.Query.Pair ("response-expires", (DateTime.to_query f)));
           Aws.Util.option_map v.response_content_type
             (fun f ->
                Aws.Query.Pair ("response-content-type", (String.to_query f)));
           Aws.Util.option_map v.response_content_language
             (fun f ->
                Aws.Query.Pair
                  ("response-content-language", (String.to_query f)));
           Aws.Util.option_map v.response_content_encoding
             (fun f ->
                Aws.Query.Pair
                  ("response-content-encoding", (String.to_query f)));
           Aws.Util.option_map v.response_content_disposition
             (fun f ->
                Aws.Query.Pair
                  ("response-content-disposition", (String.to_query f)));
           Aws.Util.option_map v.response_cache_control
             (fun f ->
                Aws.Query.Pair
                  ("response-cache-control", (String.to_query f)));
           Aws.Util.option_map v.range
             (fun f -> Aws.Query.Pair ("Range", (String.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Aws.Util.option_map v.if_unmodified_since
             (fun f ->
                Aws.Query.Pair ("If-Unmodified-Since", (DateTime.to_query f)));
           Aws.Util.option_map v.if_none_match
             (fun f -> Aws.Query.Pair ("If-None-Match", (String.to_query f)));
           Aws.Util.option_map v.if_modified_since
             (fun f ->
                Aws.Query.Pair ("If-Modified-Since", (DateTime.to_query f)));
           Aws.Util.option_map v.if_match
             (fun f -> Aws.Query.Pair ("If-Match", (String.to_query f)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_mode
              (fun f -> ("x-amz-checksum-mode", (ChecksumMode.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.part_number
             (fun f -> ("partNumber", (Integer.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key
             (fun f ->
                ("x-amz-server-side-encryption-customer-key",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Aws.Util.option_map v.response_expires
             (fun f -> ("response-expires", (DateTime.to_json f)));
           Aws.Util.option_map v.response_content_type
             (fun f -> ("response-content-type", (String.to_json f)));
           Aws.Util.option_map v.response_content_language
             (fun f -> ("response-content-language", (String.to_json f)));
           Aws.Util.option_map v.response_content_encoding
             (fun f -> ("response-content-encoding", (String.to_json f)));
           Aws.Util.option_map v.response_content_disposition
             (fun f -> ("response-content-disposition", (String.to_json f)));
           Aws.Util.option_map v.response_cache_control
             (fun f -> ("response-cache-control", (String.to_json f)));
           Aws.Util.option_map v.range
             (fun f -> ("Range", (String.to_json f)));
           Some ("Key", (String.to_json v.key));
           Aws.Util.option_map v.if_unmodified_since
             (fun f -> ("If-Unmodified-Since", (DateTime.to_json f)));
           Aws.Util.option_map v.if_none_match
             (fun f -> ("If-None-Match", (String.to_json f)));
           Aws.Util.option_map v.if_modified_since
             (fun f -> ("If-Modified-Since", (DateTime.to_json f)));
           Aws.Util.option_map v.if_match
             (fun f -> ("If-Match", (String.to_json f)));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        if_match =
          (Aws.Util.option_map (Aws.Json.lookup j "If-Match") String.of_json);
        if_modified_since =
          (Aws.Util.option_map (Aws.Json.lookup j "If-Modified-Since")
             DateTime.of_json);
        if_none_match =
          (Aws.Util.option_map (Aws.Json.lookup j "If-None-Match")
             String.of_json);
        if_unmodified_since =
          (Aws.Util.option_map (Aws.Json.lookup j "If-Unmodified-Since")
             DateTime.of_json);
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        range =
          (Aws.Util.option_map (Aws.Json.lookup j "Range") String.of_json);
        response_cache_control =
          (Aws.Util.option_map (Aws.Json.lookup j "response-cache-control")
             String.of_json);
        response_content_disposition =
          (Aws.Util.option_map
             (Aws.Json.lookup j "response-content-disposition")
             String.of_json);
        response_content_encoding =
          (Aws.Util.option_map
             (Aws.Json.lookup j "response-content-encoding") String.of_json);
        response_content_language =
          (Aws.Util.option_map
             (Aws.Json.lookup j "response-content-language") String.of_json);
        response_content_type =
          (Aws.Util.option_map (Aws.Json.lookup j "response-content-type")
             String.of_json);
        response_expires =
          (Aws.Util.option_map (Aws.Json.lookup j "response-expires")
             DateTime.of_json);
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-customer-key")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        part_number =
          (Aws.Util.option_map (Aws.Json.lookup j "partNumber")
             Integer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        checksum_mode =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-mode")
             ChecksumMode.of_json)
      }
  end
module GetBucketAnalyticsConfigurationOutput =
  struct
    type t = {
      analytics_configuration: AnalyticsConfiguration.t option }
    let make ?analytics_configuration  () = { analytics_configuration }
    let parse xml =
      Some
        {
          analytics_configuration =
            (Aws.Util.option_bind
               (Aws.Xml.member "AnalyticsConfiguration" xml)
               AnalyticsConfiguration.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.analytics_configuration
              (fun f ->
                 Aws.Query.Pair
                   ("AnalyticsConfiguration",
                     (AnalyticsConfiguration.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.analytics_configuration
              (fun f ->
                 ("AnalyticsConfiguration",
                   (AnalyticsConfiguration.to_json f)))])
    let of_json j =
      {
        analytics_configuration =
          (Aws.Util.option_map (Aws.Json.lookup j "AnalyticsConfiguration")
             AnalyticsConfiguration.of_json)
      }
  end
module CreateMultipartUploadOutput =
  struct
    type t =
      {
      abort_date: DateTime.t option ;
      abort_rule_id: String.t option ;
      bucket: String.t option ;
      key: String.t option ;
      upload_id: String.t option ;
      server_side_encryption: ServerSideEncryption.t option ;
      s_s_e_customer_algorithm: String.t option ;
      s_s_e_customer_key_m_d5: String.t option ;
      s_s_e_k_m_s_key_id: String.t option ;
      s_s_e_k_m_s_encryption_context: String.t option ;
      bucket_key_enabled: Boolean.t option ;
      request_charged: RequestCharged.t option ;
      checksum_algorithm: ChecksumAlgorithm.t option }
    let make ?abort_date  ?abort_rule_id  ?bucket  ?key  ?upload_id 
      ?server_side_encryption  ?s_s_e_customer_algorithm 
      ?s_s_e_customer_key_m_d5  ?s_s_e_k_m_s_key_id 
      ?s_s_e_k_m_s_encryption_context  ?bucket_key_enabled  ?request_charged 
      ?checksum_algorithm  () =
      {
        abort_date;
        abort_rule_id;
        bucket;
        key;
        upload_id;
        server_side_encryption;
        s_s_e_customer_algorithm;
        s_s_e_customer_key_m_d5;
        s_s_e_k_m_s_key_id;
        s_s_e_k_m_s_encryption_context;
        bucket_key_enabled;
        request_charged;
        checksum_algorithm
      }
    let parse xml =
      Some
        {
          abort_date =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-abort-date" xml)
               DateTime.parse);
          abort_rule_id =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-abort-rule-id" xml)
               String.parse);
          bucket =
            (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml) String.parse);
          key =
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse);
          upload_id =
            (Aws.Util.option_bind (Aws.Xml.member "UploadId" xml)
               String.parse);
          server_side_encryption =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption" xml)
               ServerSideEncryption.parse);
          s_s_e_customer_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-algorithm" xml)
               String.parse);
          s_s_e_customer_key_m_d5 =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-customer-key-MD5" xml)
               String.parse);
          s_s_e_k_m_s_key_id =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-aws-kms-key-id"
                  xml) String.parse);
          s_s_e_k_m_s_encryption_context =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-server-side-encryption-context" xml)
               String.parse);
          bucket_key_enabled =
            (Aws.Util.option_bind
               (Aws.Xml.member
                  "x-amz-server-side-encryption-bucket-key-enabled" xml)
               Boolean.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse);
          checksum_algorithm =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-checksum-algorithm" xml)
               ChecksumAlgorithm.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_algorithm
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-checksum-algorithm",
                     (ChecksumAlgorithm.to_query f)));
           Aws.Util.option_map v.request_charged
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-bucket-key-enabled",
                    (Boolean.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_encryption_context
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-context",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-aws-kms-key-id",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-key-MD5",
                    (String.to_query f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption-customer-algorithm",
                    (String.to_query f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-server-side-encryption",
                    (ServerSideEncryption.to_query f)));
           Aws.Util.option_map v.upload_id
             (fun f -> Aws.Query.Pair ("UploadId", (String.to_query f)));
           Aws.Util.option_map v.key
             (fun f -> Aws.Query.Pair ("Key", (String.to_query f)));
           Aws.Util.option_map v.bucket
             (fun f -> Aws.Query.Pair ("Bucket", (String.to_query f)));
           Aws.Util.option_map v.abort_rule_id
             (fun f ->
                Aws.Query.Pair ("x-amz-abort-rule-id", (String.to_query f)));
           Aws.Util.option_map v.abort_date
             (fun f ->
                Aws.Query.Pair ("x-amz-abort-date", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.checksum_algorithm
              (fun f ->
                 ("x-amz-checksum-algorithm", (ChecksumAlgorithm.to_json f)));
           Aws.Util.option_map v.request_charged
             (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.bucket_key_enabled
             (fun f ->
                ("x-amz-server-side-encryption-bucket-key-enabled",
                  (Boolean.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_encryption_context
             (fun f ->
                ("x-amz-server-side-encryption-context", (String.to_json f)));
           Aws.Util.option_map v.s_s_e_k_m_s_key_id
             (fun f ->
                ("x-amz-server-side-encryption-aws-kms-key-id",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_key_m_d5
             (fun f ->
                ("x-amz-server-side-encryption-customer-key-MD5",
                  (String.to_json f)));
           Aws.Util.option_map v.s_s_e_customer_algorithm
             (fun f ->
                ("x-amz-server-side-encryption-customer-algorithm",
                  (String.to_json f)));
           Aws.Util.option_map v.server_side_encryption
             (fun f ->
                ("x-amz-server-side-encryption",
                  (ServerSideEncryption.to_json f)));
           Aws.Util.option_map v.upload_id
             (fun f -> ("UploadId", (String.to_json f)));
           Aws.Util.option_map v.key (fun f -> ("Key", (String.to_json f)));
           Aws.Util.option_map v.bucket
             (fun f -> ("Bucket", (String.to_json f)));
           Aws.Util.option_map v.abort_rule_id
             (fun f -> ("x-amz-abort-rule-id", (String.to_json f)));
           Aws.Util.option_map v.abort_date
             (fun f -> ("x-amz-abort-date", (DateTime.to_json f)))])
    let of_json j =
      {
        abort_date =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-abort-date")
             DateTime.of_json);
        abort_rule_id =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-abort-rule-id")
             String.of_json);
        bucket =
          (Aws.Util.option_map (Aws.Json.lookup j "Bucket") String.of_json);
        key = (Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json);
        upload_id =
          (Aws.Util.option_map (Aws.Json.lookup j "UploadId") String.of_json);
        server_side_encryption =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption")
             ServerSideEncryption.of_json);
        s_s_e_customer_algorithm =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-algorithm")
             String.of_json);
        s_s_e_customer_key_m_d5 =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-customer-key-MD5")
             String.of_json);
        s_s_e_k_m_s_key_id =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-aws-kms-key-id")
             String.of_json);
        s_s_e_k_m_s_encryption_context =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-server-side-encryption-context")
             String.of_json);
        bucket_key_enabled =
          (Aws.Util.option_map
             (Aws.Json.lookup j
                "x-amz-server-side-encryption-bucket-key-enabled")
             Boolean.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json);
        checksum_algorithm =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-checksum-algorithm")
             ChecksumAlgorithm.of_json)
      }
  end
module PutObjectRetentionOutput =
  struct
    type t = {
      request_charged: RequestCharged.t option }
    let make ?request_charged  () = { request_charged }
    let parse xml =
      Some
        {
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)))])
    let of_json j =
      {
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end
module GetObjectLegalHoldRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      version_id: String.t option ;
      request_payer: RequestPayer.t option ;
      expected_bucket_owner: String.t option }
    let make ~bucket  ~key  ?version_id  ?request_payer 
      ?expected_bucket_owner  () =
      { bucket; key; version_id; request_payer; expected_bucket_owner }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.request_payer
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.expected_bucket_owner
              (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.request_payer
             (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json)
      }
  end
module GetObjectTaggingRequest =
  struct
    type t =
      {
      bucket: String.t ;
      key: String.t ;
      version_id: String.t option ;
      expected_bucket_owner: String.t option ;
      request_payer: RequestPayer.t option }
    let make ~bucket  ~key  ?version_id  ?expected_bucket_owner 
      ?request_payer  () =
      { bucket; key; version_id; expected_bucket_owner; request_payer }
    let parse xml =
      Some
        {
          bucket =
            (Aws.Xml.required "Bucket"
               (Aws.Util.option_bind (Aws.Xml.member "Bucket" xml)
                  String.parse));
          key =
            (Aws.Xml.required "Key"
               (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse));
          version_id =
            (Aws.Util.option_bind (Aws.Xml.member "versionId" xml)
               String.parse);
          expected_bucket_owner =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-expected-bucket-owner" xml)
               String.parse);
          request_payer =
            (Aws.Util.option_bind (Aws.Xml.member "x-amz-request-payer" xml)
               RequestPayer.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_payer
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-payer", (RequestPayer.to_query f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f ->
                Aws.Query.Pair
                  ("x-amz-expected-bucket-owner", (String.to_query f)));
           Aws.Util.option_map v.version_id
             (fun f -> Aws.Query.Pair ("versionId", (String.to_query f)));
           Some (Aws.Query.Pair ("Key", (String.to_query v.key)));
           Some (Aws.Query.Pair ("Bucket", (String.to_query v.bucket)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_payer
              (fun f -> ("x-amz-request-payer", (RequestPayer.to_json f)));
           Aws.Util.option_map v.expected_bucket_owner
             (fun f -> ("x-amz-expected-bucket-owner", (String.to_json f)));
           Aws.Util.option_map v.version_id
             (fun f -> ("versionId", (String.to_json f)));
           Some ("Key", (String.to_json v.key));
           Some ("Bucket", (String.to_json v.bucket))])
    let of_json j =
      {
        bucket =
          (String.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Bucket")));
        key =
          (String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key")));
        version_id =
          (Aws.Util.option_map (Aws.Json.lookup j "versionId") String.of_json);
        expected_bucket_owner =
          (Aws.Util.option_map
             (Aws.Json.lookup j "x-amz-expected-bucket-owner") String.of_json);
        request_payer =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-payer")
             RequestPayer.of_json)
      }
  end
module ListObjectsV2Output =
  struct
    type t =
      {
      is_truncated: Boolean.t option ;
      contents: ObjectList.t ;
      name: String.t option ;
      prefix: String.t option ;
      delimiter: String.t option ;
      max_keys: Integer.t option ;
      common_prefixes: CommonPrefixList.t ;
      encoding_type: EncodingType.t option ;
      key_count: Integer.t option ;
      continuation_token: String.t option ;
      next_continuation_token: String.t option ;
      start_after: String.t option ;
      request_charged: RequestCharged.t option }
    let make ?is_truncated  ?(contents= [])  ?name  ?prefix  ?delimiter 
      ?max_keys  ?(common_prefixes= [])  ?encoding_type  ?key_count 
      ?continuation_token  ?next_continuation_token  ?start_after 
      ?request_charged  () =
      {
        is_truncated;
        contents;
        name;
        prefix;
        delimiter;
        max_keys;
        common_prefixes;
        encoding_type;
        key_count;
        continuation_token;
        next_continuation_token;
        start_after;
        request_charged
      }
    let parse xml =
      Some
        {
          is_truncated =
            (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml)
               Boolean.parse);
          contents = (Aws.Util.of_option [] (ObjectList.parse xml));
          name =
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse);
          prefix =
            (Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse);
          delimiter =
            (Aws.Util.option_bind (Aws.Xml.member "Delimiter" xml)
               String.parse);
          max_keys =
            (Aws.Util.option_bind (Aws.Xml.member "MaxKeys" xml)
               Integer.parse);
          common_prefixes =
            (Aws.Util.of_option [] (CommonPrefixList.parse xml));
          encoding_type =
            (Aws.Util.option_bind (Aws.Xml.member "EncodingType" xml)
               EncodingType.parse);
          key_count =
            (Aws.Util.option_bind (Aws.Xml.member "KeyCount" xml)
               Integer.parse);
          continuation_token =
            (Aws.Util.option_bind (Aws.Xml.member "ContinuationToken" xml)
               String.parse);
          next_continuation_token =
            (Aws.Util.option_bind
               (Aws.Xml.member "NextContinuationToken" xml) String.parse);
          start_after =
            (Aws.Util.option_bind (Aws.Xml.member "StartAfter" xml)
               String.parse);
          request_charged =
            (Aws.Util.option_bind
               (Aws.Xml.member "x-amz-request-charged" xml)
               RequestCharged.parse)
        }
    let to_query v =
      Aws.Query.List
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f ->
                 Aws.Query.Pair
                   ("x-amz-request-charged", (RequestCharged.to_query f)));
           Aws.Util.option_map v.start_after
             (fun f -> Aws.Query.Pair ("StartAfter", (String.to_query f)));
           Aws.Util.option_map v.next_continuation_token
             (fun f ->
                Aws.Query.Pair ("NextContinuationToken", (String.to_query f)));
           Aws.Util.option_map v.continuation_token
             (fun f ->
                Aws.Query.Pair ("ContinuationToken", (String.to_query f)));
           Aws.Util.option_map v.key_count
             (fun f -> Aws.Query.Pair ("KeyCount", (Integer.to_query f)));
           Aws.Util.option_map v.encoding_type
             (fun f ->
                Aws.Query.Pair ("EncodingType", (EncodingType.to_query f)));
           Some
             (Aws.Query.Pair
                ("CommonPrefixes.member",
                  (CommonPrefixList.to_query v.common_prefixes)));
           Aws.Util.option_map v.max_keys
             (fun f -> Aws.Query.Pair ("MaxKeys", (Integer.to_query f)));
           Aws.Util.option_map v.delimiter
             (fun f -> Aws.Query.Pair ("Delimiter", (String.to_query f)));
           Aws.Util.option_map v.prefix
             (fun f -> Aws.Query.Pair ("Prefix", (String.to_query f)));
           Aws.Util.option_map v.name
             (fun f -> Aws.Query.Pair ("Name", (String.to_query f)));
           Some
             (Aws.Query.Pair
                ("Contents.member", (ObjectList.to_query v.contents)));
           Aws.Util.option_map v.is_truncated
             (fun f -> Aws.Query.Pair ("IsTruncated", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Aws.Util.list_filter_opt
           [Aws.Util.option_map v.request_charged
              (fun f -> ("x-amz-request-charged", (RequestCharged.to_json f)));
           Aws.Util.option_map v.start_after
             (fun f -> ("StartAfter", (String.to_json f)));
           Aws.Util.option_map v.next_continuation_token
             (fun f -> ("NextContinuationToken", (String.to_json f)));
           Aws.Util.option_map v.continuation_token
             (fun f -> ("ContinuationToken", (String.to_json f)));
           Aws.Util.option_map v.key_count
             (fun f -> ("KeyCount", (Integer.to_json f)));
           Aws.Util.option_map v.encoding_type
             (fun f -> ("EncodingType", (EncodingType.to_json f)));
           Some
             ("CommonPrefixes", (CommonPrefixList.to_json v.common_prefixes));
           Aws.Util.option_map v.max_keys
             (fun f -> ("MaxKeys", (Integer.to_json f)));
           Aws.Util.option_map v.delimiter
             (fun f -> ("Delimiter", (String.to_json f)));
           Aws.Util.option_map v.prefix
             (fun f -> ("Prefix", (String.to_json f)));
           Aws.Util.option_map v.name (fun f -> ("Name", (String.to_json f)));
           Some ("Contents", (ObjectList.to_json v.contents));
           Aws.Util.option_map v.is_truncated
             (fun f -> ("IsTruncated", (Boolean.to_json f)))])
    let of_json j =
      {
        is_truncated =
          (Aws.Util.option_map (Aws.Json.lookup j "IsTruncated")
             Boolean.of_json);
        contents =
          (ObjectList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "Contents")));
        name =
          (Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json);
        prefix =
          (Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json);
        delimiter =
          (Aws.Util.option_map (Aws.Json.lookup j "Delimiter") String.of_json);
        max_keys =
          (Aws.Util.option_map (Aws.Json.lookup j "MaxKeys") Integer.of_json);
        common_prefixes =
          (CommonPrefixList.of_json
             (Aws.Util.of_option_exn (Aws.Json.lookup j "CommonPrefixes")));
        encoding_type =
          (Aws.Util.option_map (Aws.Json.lookup j "EncodingType")
             EncodingType.of_json);
        key_count =
          (Aws.Util.option_map (Aws.Json.lookup j "KeyCount") Integer.of_json);
        continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "ContinuationToken")
             String.of_json);
        next_continuation_token =
          (Aws.Util.option_map (Aws.Json.lookup j "NextContinuationToken")
             String.of_json);
        start_after =
          (Aws.Util.option_map (Aws.Json.lookup j "StartAfter")
             String.of_json);
        request_charged =
          (Aws.Util.option_map (Aws.Json.lookup j "x-amz-request-charged")
             RequestCharged.of_json)
      }
  end