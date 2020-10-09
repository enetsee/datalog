module type S = sig
  include Schedule.ScheduleM.S

  val err_no_ordering : Core.Binding.t -> Reporting.Region.t -> _ t
end
