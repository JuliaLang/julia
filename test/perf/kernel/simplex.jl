const Basic = 1
const AtLower = 2
const AtUpper = 3
const dualTol = 1e-7

# Adapted from https://github.com/mlubin/SimplexBenchmarks.
# See that project for a suite of cross-language benchmarks.

# This code performs a two-pass "Harris" ratio test used within the
# dual simplex algorithm for linear programming.
# See the thesis of Achim Koberstein,
# "The dual simplex method, techniques for a fast and stable implementation"
# 2005, Section 6.2.2.2.

# Data taken from sample greenbea iteration
function doTwoPassRatioTest()

    n = 8000

    candidates = zeros(Int,n) # don't count allocation time, assume reuse
    thetaMax = 1e25
    pivotTol = 1e-7

    red1 = [144.96711464551225,1.1799262694258228,15.258941028685781,0.0,74.21362490887168,8.771366223499031,0.0,0.0,6.819776136699029,0.0,-25.47923898089684,2.5194527603132846,74.99857065834307,0.0,0.0,88.42327610251432,0.6820330826047487,3.3715482573829685,0.0,0.0]
    var1 = [3,2,2,1,2,2,1,1,2,1,3,2,2,1,1,2,2,2,1,2]
    tab1 = [-3.823733185508287,0.7857013769778555,9.347700223333298,-0.0,-24.57958470726409,3.549760868834472,-0.0,-0.0,-0.0,-0.0,2.276570692386853,-3.1513940897258808,6.600120188297597,-0.0,-0.0,2.483577554811755,-0.5411982936821893,-0.20714710316669951,-0.0,-0.0]

    redcost = repmat(red1,400,1)
    varstate = repmat(var1,400,1)
    tabrow = repmat(tab1,400,1)

    t = time()
    for k in 1:1000
        ncandidates = 0
        for i in 1:n
            thisState = varstate[i]
            pivotElt = tabrow[i]
            if (thisState == AtLower && pivotElt > pivotTol) || (thisState == AtUpper && pivotElt < -pivotTol)
                candidates[ncandidates += 1] = i
                ratio = 0.
                if (pivotElt < 0.)
                    ratio = (redcost[i] - dualTol)/pivotElt
                else
                    ratio = (redcost[i] + dualTol)/pivotElt
                end
                if (ratio < thetaMax)
                    thetaMax = ratio
                end
            end
        end

        # pass 2
        enter = -1
        maxAlpha = 0.
        for k in 1:ncandidates
            i = candidates[k]
            ratio = redcost[i]/tabrow[i]
            if (ratio <= thetaMax)
                absalpha = abs(tabrow[i])
                if (absalpha > maxAlpha)
                    maxAlpha = absalpha
                    enter = i
                end
            end
        end
    end

    return time() - t

end
