unit _umtx;

{$mode ObjFPC}{$H+}

interface

type
 p_umtx=^umtx;
 umtx=packed record
  u_owner   :QWORD;                // Owner of the mutex
 end;

 p_umutex=^umutex;
 umutex=packed record
  m_owner   :DWORD;                // Owner of the mutex
  m_flags   :DWORD;                // Flags of the mutex
  m_ceilings:array[0..1] of DWORD; // Priority protect ceiling
  m_spare   :array[0..1] of QWORD;
 end;

 p_ucond=^ucond;
 ucond=packed record
  c_has_waiters:DWORD; // Has waiters in kernel
  c_flags      :DWORD; // Flags of the condition variable
  c_clockid    :DWORD; // Clock id
  c_spare      :QWORD; // Spare space
 end;

 p_urwlock=^urwlock;
 urwlock=packed record
  rw_state          :Integer;
  rw_flags          :DWORD;
  rw_blocked_readers:DWORD;
  rw_blocked_writers:DWORD;
  rw_spare          :array[0..3] of DWORD;
 end;

 p__usem=^_usem;
 _usem=packed record
  _has_waiters:DWORD;
  _count      :DWORD;
  _flags      :DWORD;
 end;

 p_umtx_op_args=^_umtx_op_args;
 _umtx_op_args=packed record
  obj   :Pointer;
  op    :Integer;
  _a    :Integer;
  val   :QWORD;
  uaddr1:Pointer;
  uaddr2:Pointer;
 end;

const
 UMTX_UNOWNED  =$0;
 UMTX_CONTESTED:QWORD=$8000000000000000;

 USYNC_PROCESS_SHARED=$0001; // Process shared sync objs

 UMUTEX_UNOWNED  =$0;
 UMUTEX_CONTESTED=$80000000;

 UMUTEX_ERROR_CHECK =$0002; // Error-checking mutex
 UMUTEX_PRIO_INHERIT=$0004; // Priority inherited mutex
 UMUTEX_PRIO_PROTECT=$0008; // Priority protect mutex

// urwlock flags
 URWLOCK_PREFER_READER=$0002;

 URWLOCK_WRITE_OWNER  =$80000000;
 URWLOCK_WRITE_WAITERS=$40000000;
 URWLOCK_READ_WAITERS =$20000000;
 URWLOCK_MAX_READERS  =$1fffffff;

 // _usem flags
 SEM_NAMED=$0002;

 // op code for _umtx_op
 UMTX_OP_LOCK             = 0;
 UMTX_OP_UNLOCK           = 1;
 UMTX_OP_WAIT             = 2;
 UMTX_OP_WAKE             = 3;
 UMTX_OP_MUTEX_TRYLOCK    = 4;
 UMTX_OP_MUTEX_LOCK       = 5;
 UMTX_OP_MUTEX_UNLOCK     = 6;
 UMTX_OP_SET_CEILING      = 7;
 UMTX_OP_CV_WAIT          = 8;
 UMTX_OP_CV_SIGNAL        = 9;
 UMTX_OP_CV_BROADCAST     =10;
 UMTX_OP_WAIT_UINT        =11;
 UMTX_OP_RW_RDLOCK        =12;
 UMTX_OP_RW_WRLOCK        =13;
 UMTX_OP_RW_UNLOCK        =14;
 UMTX_OP_WAIT_UINT_PRIVATE=15;
 UMTX_OP_WAKE_PRIVATE     =16;
 UMTX_OP_MUTEX_WAIT       =17;
 UMTX_OP_MUTEX_WAKE       =18; // deprecated
 UMTX_OP_SEM_WAIT         =19;
 UMTX_OP_SEM_WAKE         =20;
 UMTX_OP_NWAKE_PRIVATE    =21;
 UMTX_OP_MUTEX_WAKE2      =22;
 UMTX_OP_MAX              =23;

 // Flags for UMTX_OP_CV_WAIT
 CVWAIT_CHECK_UNPARKING=$01;
 CVWAIT_ABSTIME        =$02;
 CVWAIT_CLOCKID        =$04;

 UMTX_CHECK_UNPARKING=CVWAIT_CHECK_UNPARKING;

 TYPE_SIMPLE_WAIT  =0;
 TYPE_CV           =1;
 TYPE_SEM          =2;
 TYPE_SIMPLE_LOCK  =3;
 TYPE_NORMAL_UMUTEX=4;
 TYPE_PI_UMUTEX    =5;
 TYPE_PP_UMUTEX    =6;
 TYPE_RWLOCK       =7;
 TYPE_FUTEX        =8;

implementation

end.

