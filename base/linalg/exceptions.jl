export LAPACKException,
       ARPACKException,
       SingularException,
       PosDefException,
       RankDeficientException

type LAPACKException <: Exception
    info::BlasInt
end

type ARPACKException <: Exception
    info::BlasInt
end

type SingularException <: Exception
    info::BlasInt
end

type PosDefException <: Exception
    info::BlasInt
end

type RankDeficientException <: Exception
    info::BlasInt
end
