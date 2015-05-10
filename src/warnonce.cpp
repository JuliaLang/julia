#include "julia.h"
#include "julia_internal.h"

#include <set>
#include <vector>

std::set<std::vector<size_t> > wo_intersect;
std::set<std::vector<size_t> > wo_subtype;
std::set<std::vector<size_t> > wo_morespecific;
std::set<std::vector<size_t> > wo_typematch;

extern "C" {

int warnonce_intersect(int var, uintptr_t ha, uintptr_t hb)
{
    std::vector<size_t> v(3);
    v[0] = (size_t) var;
    v[1] = (size_t) ha;
    v[2] = (size_t) hb;
    std::set<std::vector<size_t> >::iterator it;
    it = wo_intersect.find(v);
    if (it == wo_intersect.end()) {
        wo_intersect.insert(v);
        return 1;
    }
    return 0;
}

int warnonce_subtype(int ta, int invariant, uintptr_t ha, uintptr_t hb)
{
    std::vector<size_t> v(4);
    v[0] = (size_t) ta;
    v[1] = (size_t) invariant;
    v[2] = (size_t) ha;
    v[3] = (size_t) hb;
    std::set<std::vector<size_t> >::iterator it;
    it = wo_subtype.find(v);
    if (it == wo_subtype.end()) {
        wo_subtype.insert(v);
        return 1;
    }
    return 0;
}

int warnonce_morespecific(int invariant, uintptr_t ha, uintptr_t hb)
{
    std::vector<size_t> v(3);
    v[0] = (size_t) invariant;
    v[1] = (size_t) ha;
    v[2] = (size_t) hb;
    std::set<std::vector<size_t> >::iterator it;
    it = wo_morespecific.find(v);
    if (it == wo_morespecific.end()) {
        wo_morespecific.insert(v);
        return 1;
    }
    return 0;
}

int warnonce_typematch(int morespecific, int invariant, uintptr_t ha, uintptr_t hb)
{
    std::vector<size_t> v(4);
    v[0] = (size_t) morespecific;
    v[1] = (size_t) invariant;
    v[2] = (size_t) ha;
    v[3] = (size_t) hb;
    std::set<std::vector<size_t> >::iterator it;
    it = wo_typematch.find(v);
    if (it == wo_typematch.end()) {
        wo_typematch.insert(v);
        return 1;
    }
    return 0;
}

}
