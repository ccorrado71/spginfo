module prog_constants

!   Message Window codes [1-6]
    enum, bind(c) 
      enumerator :: INFO_WINDOW = 1, QUEST_WINDOW, QUEST_WINDOW_R,          &
      QUEST_WINDOW_YES_NO, QUEST_WINDOW_NO_YES, THREE_BUTTONS, WARN_WINDOW, &
      ERR_WINDOW, SEVERE_ERR_WINDOW
    endenum
!
!  Message error code
   enum, bind(c)
     enumerator :: ERR_MISSING_SPG = 1001, ERR_UNKNOWN_SPG, ERR_CELL_PARAM, ERR_STRUCTURE
   endenum

   real, parameter :: CU_WAVE = 1.540560
   real, parameter :: DEF_WAVE = CU_WAVE
   real, parameter :: AU_TO_ANG = 0.529177249

end module prog_constants

module trig_constants
!  Trigonometric constants
   real, parameter :: pi     = 4.0*atan(1.0)     ! pi
   real, parameter :: twopi  = 2.0*pi            ! 2*pi
   real, parameter :: twopis = 2.0*pi*pi         ! 2*pi*pi
   real, parameter :: rtod   = 180.0/pi          ! radiant to degree
   real, parameter :: dtor   = 1.0/rtod          ! degree to radiant
end module trig_constants

module type_constants
   integer, parameter  :: I4B = selected_int_kind(9)   ! long integer
   integer, parameter  :: SP  = kind(1.0)              ! single precision
   integer, parameter  :: DP  = kind(1.0d0)            ! double precision

   real, parameter     :: epsmch  = epsilon(epsmch)
   real(DP), parameter :: epsmchd = epsilon(epsmchd)
end module type_constants
