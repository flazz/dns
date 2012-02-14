-record(header,
  { id, qr, opcode, authoritative_answer, truncation,
    recursion_desired, recursion_available,
    rcode, qcount, acount, nscount, arcount }).

-define(QR_QUERY, 0).
-define(QR_RESPONSE, 1).

-define(OPCODE_QUERY, 0).
-define(OPCODE_IQUERY, 1).
-define(OPCODE_STATUS, 2).

-define(AUTHORITATIVE_ANSWER, 1).
-define(NON_AUTHORITATIVE_ANSWER, 0).

-define(NO_TRUNCATION, 0).
-define(TRUNCATION, 1).

-define(NO_RECURSION_AVAILABLE, 0).
-define(RECURSION_AVAILABLE, 1).

-define(RCODE_NO_ERROR, 0).
-define(RCODE_FORMAT_ERROR, 1).
-define(RCODE_SERVER_FAILURE, 2).
-define(RCODE_NAME_ERROR, 3).
-define(RCODE_NOT_IMPLEMENTED, 4).
-define(RCODE_REFUSED, 5).

-define(CLASS_IN, 1).
-define(CLASS_CS, 2).
-define(CLASS_CH, 3).
-define(CLASS_HS, 4).
-define(Q_CLASS_ANY, 255).

-define(TYPE_A, 1).
-define(TYPE_NS, 2).
-define(TYPE_MD, 3).
-define(TYPE_MF, 4).
-define(TYPE_CNAME, 5).
-define(TYPE_SOA, 6).
-define(TYPE_MB, 7).
-define(TYPE_MG, 8).
-define(TYPE_MR, 9).
-define(TYPE_NULL, 10).
-define(TYPE_WKS, 11).
-define(TYPE_PTR, 12).
-define(TYPE_HINFO, 13).
-define(TYPE_MINFO, 14).
-define(TYPE_MX, 15).
-define(TYPE_TXT, 16).
-define(Q_TYPE_AXFR, 252).
-define(Q_TYPE_MAILB, 253).
-define(Q_TYPE_MAILA, 254).
-define(Q_TYPE_ANY, 255).
