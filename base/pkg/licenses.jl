module Licenses

type License
    abbreviation::String
    name::String
    terms::String # Markdown formatted terms with pkg and copyright info
    wiki::String
end


asl() = License("ASL", "Apache License, Version 2.0", "", "http://en.wikipedia.org/wiki/Apache_License")
function asl(pkg::String, copyright::String)
    lic = asl()
    lic.terms = asl_terms(pkg,copyright)
    return lic
end


bsd() = License("BSD", "BSD 2-Clause License", "", "http://en.wikipedia.org/wiki/BSD_licenses")
function bsd(pkg::String, copyright::String)
    lic = bsd()
    lic.terms = bsd_terms(pkg,copyright)
    return lic
end


mit() = License("MIT", "MIT \"Expat\" License", "", "http://en.wikipedia.org/wiki/MIT_License")
function mit(pkg::String, copyright::String)
    lic = mit()
    lic.terms = mit_terms(pkg,copyright)
    return lic
end

gpl2() = License("GPLv2", "GNU General Public License, version 2", "", "http://en.wikipedia.org/wiki/GNU_General_Public_License#Version_2")
function gpl2(pkg::String, copyright::String)
    lic = gpl2()
    lic.terms = gpl2_terms(pkg,copyright)
    return lic
end

lgpl2() = License("LGPLv2", "GNU Lesser General Public License, version 2", "", "http://en.wikipedia.org/wiki/GNU_Lesser_General_Public_License")
function lgpl2(pkg::String, copyright::String)
    lic = lgpl2()
    lic.terms = lgpl2_terms(pkg,copyright)
    return lic
end

cc0() = License("CC0", "CC0 1.0 Universal Public Domain Dedication", "", "http://en.wikipedia.org/wiki/Creative_Commons_license#Zero_.2F_Public_domain")
function cc0(pkg::String, copyright::String)
    lic = cc0()
    lic.terms = cc0_terms(pkg,copyright)
    return lic
end


# http://www.apache.org/licenses/LICENSE-2.0
asl_terms(pkg::String, copyright::String) =
"""
The $pkg.jl package is licensed under version 2.0 of the Apache License:

> $(copyright)
>
>                                 Apache License
>                           Version 2.0, January 2004
>                        http://www.apache.org/licenses/
>
>   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION
>
>   1. Definitions.
>
>      "License" shall mean the terms and conditions for use, reproduction,
>      and distribution as defined by Sections 1 through 9 of this document.
>
>      "Licensor" shall mean the copyright owner or entity authorized by
>      the copyright owner that is granting the License.
>
>      "Legal Entity" shall mean the union of the acting entity and all
>      other entities that control, are controlled by, or are under common
>      control with that entity. For the purposes of this definition,
>      "control" means (i) the power, direct or indirect, to cause the
>      direction or management of such entity, whether by contract or
>      otherwise, or (ii) ownership of fifty percent (50%) or more of the
>      outstanding shares, or (iii) beneficial ownership of such entity.
>
>      "You" (or "Your") shall mean an individual or Legal Entity
>      exercising permissions granted by this License.
>
>      "Source" form shall mean the preferred form for making modifications,
>      including but not limited to software source code, documentation
>      source, and configuration files.
>
>      "Object" form shall mean any form resulting from mechanical
>      transformation or translation of a Source form, including but
>      not limited to compiled object code, generated documentation,
>      and conversions to other media types.
>
>      "Work" shall mean the work of authorship, whether in Source or
>      Object form, made available under the License, as indicated by a
>      copyright notice that is included in or attached to the work
>      (an example is provided in the Appendix below).
>
>      "Derivative Works" shall mean any work, whether in Source or Object
>      form, that is based on (or derived from) the Work and for which the
>      editorial revisions, annotations, elaborations, or other modifications
>      represent, as a whole, an original work of authorship. For the purposes
>      of this License, Derivative Works shall not include works that remain
>      separable from, or merely link (or bind by name) to the interfaces of,
>      the Work and Derivative Works thereof.
>
>      "Contribution" shall mean any work of authorship, including
>      the original version of the Work and any modifications or additions
>      to that Work or Derivative Works thereof, that is intentionally
>      submitted to Licensor for inclusion in the Work by the copyright owner
>      or by an individual or Legal Entity authorized to submit on behalf of
>      the copyright owner. For the purposes of this definition, "submitted"
>      means any form of electronic, verbal, or written communication sent
>      to the Licensor or its representatives, including but not limited to
>      communication on electronic mailing lists, source code control systems,
>      and issue tracking systems that are managed by, or on behalf of, the
>      Licensor for the purpose of discussing and improving the Work, but
>      excluding communication that is conspicuously marked or otherwise
>      designated in writing by the copyright owner as "Not a Contribution."
>
>      "Contributor" shall mean Licensor and any individual or Legal Entity
>      on behalf of whom a Contribution has been received by Licensor and
>      subsequently incorporated within the Work.
>
>   2. Grant of Copyright License. Subject to the terms and conditions of
>      this License, each Contributor hereby grants to You a perpetual,
>      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
>      copyright license to reproduce, prepare Derivative Works of,
>      publicly display, publicly perform, sublicense, and distribute the
>      Work and such Derivative Works in Source or Object form.
>
>   3. Grant of Patent License. Subject to the terms and conditions of
>      this License, each Contributor hereby grants to You a perpetual,
>      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
>      (except as stated in this section) patent license to make, have made,
>      use, offer to sell, sell, import, and otherwise transfer the Work,
>      where such license applies only to those patent claims licensable
>      by such Contributor that are necessarily infringed by their
>      Contribution(s) alone or by combination of their Contribution(s)
>      with the Work to which such Contribution(s) was submitted. If You
>      institute patent litigation against any entity (including a
>      cross-claim or counterclaim in a lawsuit) alleging that the Work
>      or a Contribution incorporated within the Work constitutes direct
>      or contributory patent infringement, then any patent licenses
>      granted to You under this License for that Work shall terminate
>      as of the date such litigation is filed.
>
>   4. Redistribution. You may reproduce and distribute copies of the
>      Work or Derivative Works thereof in any medium, with or without
>      modifications, and in Source or Object form, provided that You
>      meet the following conditions:
>
>      (a) You must give any other recipients of the Work or
>          Derivative Works a copy of this License; and
>
>      (b) You must cause any modified files to carry prominent notices
>          stating that You changed the files; and
>
>      (c) You must retain, in the Source form of any Derivative Works
>          that You distribute, all copyright, patent, trademark, and
>          attribution notices from the Source form of the Work,
>          excluding those notices that do not pertain to any part of
>          the Derivative Works; and
>
>      (d) If the Work includes a "NOTICE" text file as part of its
>          distribution, then any Derivative Works that You distribute must
>          include a readable copy of the attribution notices contained
>          within such NOTICE file, excluding those notices that do not
>          pertain to any part of the Derivative Works, in at least one
>          of the following places: within a NOTICE text file distributed
>          as part of the Derivative Works; within the Source form or
>          documentation, if provided along with the Derivative Works; or,
>          within a display generated by the Derivative Works, if and
>          wherever such third-party notices normally appear. The contents
>          of the NOTICE file are for informational purposes only and
>          do not modify the License. You may add Your own attribution
>          notices within Derivative Works that You distribute, alongside
>          or as an addendum to the NOTICE text from the Work, provided
>          that such additional attribution notices cannot be construed
>          as modifying the License.
>
>      You may add Your own copyright statement to Your modifications and
>      may provide additional or different license terms and conditions
>      for use, reproduction, or distribution of Your modifications, or
>      for any such Derivative Works as a whole, provided Your use,
>      reproduction, and distribution of the Work otherwise complies with
>      the conditions stated in this License.
>
>   5. Submission of Contributions. Unless You explicitly state otherwise,
>      any Contribution intentionally submitted for inclusion in the Work
>      by You to the Licensor shall be under the terms and conditions of
>      this License, without any additional terms or conditions.
>      Notwithstanding the above, nothing herein shall supersede or modify
>      the terms of any separate license agreement you may have executed
>      with Licensor regarding such Contributions.
>
>   6. Trademarks. This License does not grant permission to use the trade
>      names, trademarks, service marks, or product names of the Licensor,
>      except as required for reasonable and customary use in describing the
>      origin of the Work and reproducing the content of the NOTICE file.
>
>   7. Disclaimer of Warranty. Unless required by applicable law or
>      agreed to in writing, Licensor provides the Work (and each
>      Contributor provides its Contributions) on an "AS IS" BASIS,
>      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
>      implied, including, without limitation, any warranties or conditions
>      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
>      PARTICULAR PURPOSE. You are solely responsible for determining the
>      appropriateness of using or redistributing the Work and assume any
>      risks associated with Your exercise of permissions under this License.
>
>   8. Limitation of Liability. In no event and under no legal theory,
>      whether in tort (including negligence), contract, or otherwise,
>      unless required by applicable law (such as deliberate and grossly
>      negligent acts) or agreed to in writing, shall any Contributor be
>      liable to You for damages, including any direct, indirect, special,
>      incidental, or consequential damages of any character arising as a
>      result of this License or out of the use or inability to use the
>      Work (including but not limited to damages for loss of goodwill,
>      work stoppage, computer failure or malfunction, or any and all
>      other commercial damages or losses), even if such Contributor
>      has been advised of the possibility of such damages.
>
>   9. Accepting Warranty or Additional Liability. While redistributing
>      the Work or Derivative Works thereof, You may choose to offer,
>      and charge a fee for, acceptance of support, warranty, indemnity,
>      or other liability obligations and/or rights consistent with this
>      License. However, in accepting such obligations, You may act only
>      on Your own behalf and on Your sole responsibility, not on behalf
>      of any other Contributor, and only if You agree to indemnify,
>      defend, and hold each Contributor harmless for any liability
>      incurred by, or claims asserted against, such Contributor by reason
>      of your accepting any such warranty or additional liability.
"""

# http://opensource.org/licenses/BSD-2-Clause
bsd_terms(pkg::String, copyright::String) =
"""
The $pkg.jl package is licensed under the Simplified "2-clause" BSD License:

> $(copyright)
>
> Redistribution and use in source and binary forms, with or without
> modification, are permitted provided that the following conditions are
> met:
>
> 1. Redistributions of source code must retain the above copyright
>    notice, this list of conditions and the following disclaimer.
> 2. Redistributions in binary form must reproduce the above copyright
>    notice, this list of conditions and the following disclaimer in the
>    documentation and/or other materials provided with the distribution.
>
> THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
> "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
> LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
> A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
> OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
> SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
> LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
> DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
> THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
> (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
> OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""

# http://en.wikipedia.org/wiki/MIT_License
mit_terms(pkg::String, copyright::String) =
"""
The $pkg.jl package is licensed under the MIT "Expat" License:

> $(copyright)
>
> Permission is hereby granted, free of charge, to any person obtaining
> a copy of this software and associated documentation files (the
> "Software"), to deal in the Software without restriction, including
> without limitation the rights to use, copy, modify, merge, publish,
> distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so, subject to
> the following conditions:
>
> The above copyright notice and this permission notice shall be
> included in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
> IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
> CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
> TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
> SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
"""

# https://www.gnu.org/licenses/old-licenses/lgpl-2.0.html
lgpl2_terms(pkg::String, copyright::String) =
"""
The $pkg.jl package is licensed under the GNU Lesser General Public License, version 2:

> $(copyright)
>
>                   GNU LIBRARY GENERAL PUBLIC LICENSE
>                        Version 2, June 1991
>
>  Copyright (C) 1991 Free Software Foundation, Inc.
>  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
>  Everyone is permitted to copy and distribute verbatim copies
>  of this license document, but changing it is not allowed.
>
> [This is the first released version of the library GPL.  It is
>  numbered 2 because it goes with version 2 of the ordinary GPL.]
>
>                             Preamble
>
>   The licenses for most software are designed to take away your
> freedom to share and change it.  By contrast, the GNU General Public
> Licenses are intended to guarantee your freedom to share and change
> free software--to make sure the software is free for all its users.
>
>   This license, the Library General Public License, applies to some
> specially designated Free Software Foundation software, and to any
> other libraries whose authors decide to use it.  You can use it for
> your libraries, too.
>
>   When we speak of free software, we are referring to freedom, not
> price.  Our General Public Licenses are designed to make sure that you
> have the freedom to distribute copies of free software (and charge for
> this service if you wish), that you receive source code or can get it
> if you want it, that you can change the software or use pieces of it
> in new free programs; and that you know you can do these things.
>
>   To protect your rights, we need to make restrictions that forbid
> anyone to deny you these rights or to ask you to surrender the rights.
> These restrictions translate to certain responsibilities for you if
> you distribute copies of the library, or if you modify it.
>
>   For example, if you distribute copies of the library, whether gratis
> or for a fee, you must give the recipients all the rights that we gave
> you.  You must make sure that they, too, receive or can get the source
> code.  If you link a program with the library, you must provide
> complete object files to the recipients so that they can relink them
> with the library, after making changes to the library and recompiling
> it.  And you must show them these terms so they know their rights.
>
>   Our method of protecting your rights has two steps: (1) copyright
> the library, and (2) offer you this license which gives you legal
> permission to copy, distribute and/or modify the library.
>
>   Also, for each distributor's protection, we want to make certain
> that everyone understands that there is no warranty for this free
> library.  If the library is modified by someone else and passed on, we
> want its recipients to know that what they have is not the original
> version, so that any problems introduced by others will not reflect on
> the original authors' reputations.
>
>   Finally, any free program is threatened constantly by software
> patents.  We wish to avoid the danger that companies distributing free
> software will individually obtain patent licenses, thus in effect
> transforming the program into proprietary software.  To prevent this,
> we have made it clear that any patent must be licensed for everyone's
> free use or not licensed at all.
>
>   Most GNU software, including some libraries, is covered by the ordinary
> GNU General Public License, which was designed for utility programs.  This
> license, the GNU Library General Public License, applies to certain
> designated libraries.  This license is quite different from the ordinary
> one; be sure to read it in full, and don't assume that anything in it is
> the same as in the ordinary license.
>
>   The reason we have a separate public license for some libraries is that
> they blur the distinction we usually make between modifying or adding to a
> program and simply using it.  Linking a program with a library, without
> changing the library, is in some sense simply using the library, and is
> analogous to running a utility program or application program.  However, in
> a textual and legal sense, the linked executable is a combined work, a
> derivative of the original library, and the ordinary General Public License
> treats it as such.
>
>   Because of this blurred distinction, using the ordinary General
> Public License for libraries did not effectively promote software
> sharing, because most developers did not use the libraries.  We
> concluded that weaker conditions might promote sharing better.
>
>   However, unrestricted linking of non-free programs would deprive the
> users of those programs of all benefit from the free status of the
> libraries themselves.  This Library General Public License is intended to
> permit developers of non-free programs to use free libraries, while
> preserving your freedom as a user of such programs to change the free
> libraries that are incorporated in them.  (We have not seen how to achieve
> this as regards changes in header files, but we have achieved it as regards
> changes in the actual functions of the Library.)  The hope is that this
> will lead to faster development of free libraries.
>
>   The precise terms and conditions for copying, distribution and
> modification follow.  Pay close attention to the difference between a
> "work based on the library" and a "work that uses the library".  The
> former contains code derived from the library, while the latter only
> works together with the library.
>
>   Note that it is possible for a library to be covered by the ordinary
> General Public License rather than by this special one.
>
>                   GNU LIBRARY GENERAL PUBLIC LICENSE
>    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
>
>   0. This License Agreement applies to any software library which
> contains a notice placed by the copyright holder or other authorized
> party saying it may be distributed under the terms of this Library
> General Public License (also called "this License").  Each licensee is
> addressed as "you".
>
>   A "library" means a collection of software functions and/or data
> prepared so as to be conveniently linked with application programs
> (which use some of those functions and data) to form executables.
>
>   The "Library", below, refers to any such software library or work
> which has been distributed under these terms.  A "work based on the
> Library" means either the Library or any derivative work under
> copyright law: that is to say, a work containing the Library or a
> portion of it, either verbatim or with modifications and/or translated
> straightforwardly into another language.  (Hereinafter, translation is
> included without limitation in the term "modification".)
>
>   "Source code" for a work means the preferred form of the work for
> making modifications to it.  For a library, complete source code means
> all the source code for all modules it contains, plus any associated
> interface definition files, plus the scripts used to control compilation
> and installation of the library.
>
>   Activities other than copying, distribution and modification are not
> covered by this License; they are outside its scope.  The act of
> running a program using the Library is not restricted, and output from
> such a program is covered only if its contents constitute a work based
> on the Library (independent of the use of the Library in a tool for
> writing it).  Whether that is true depends on what the Library does
> and what the program that uses the Library does.
>
>   1. You may copy and distribute verbatim copies of the Library's
> complete source code as you receive it, in any medium, provided that
> you conspicuously and appropriately publish on each copy an
> appropriate copyright notice and disclaimer of warranty; keep intact
> all the notices that refer to this License and to the absence of any
> warranty; and distribute a copy of this License along with the
> Library.
>
>   You may charge a fee for the physical act of transferring a copy,
> and you may at your option offer warranty protection in exchange for a
> fee.
>
>   2. You may modify your copy or copies of the Library or any portion
> of it, thus forming a work based on the Library, and copy and
> distribute such modifications or work under the terms of Section 1
> above, provided that you also meet all of these conditions:
>
>     a) The modified work must itself be a software library.
>
>     b) You must cause the files modified to carry prominent notices
>     stating that you changed the files and the date of any change.
>
>     c) You must cause the whole of the work to be licensed at no
>     charge to all third parties under the terms of this License.
>
>     d) If a facility in the modified Library refers to a function or a
>     table of data to be supplied by an application program that uses
>     the facility, other than as an argument passed when the facility
>     is invoked, then you must make a good faith effort to ensure that,
>     in the event an application does not supply such function or
>     table, the facility still operates, and performs whatever part of
>     its purpose remains meaningful.
>
>     (For example, a function in a library to compute square roots has
>     a purpose that is entirely well-defined independent of the
>     application.  Therefore, Subsection 2d requires that any
>     application-supplied function or table used by this function must
>     be optional: if the application does not supply it, the square
>     root function must still compute square roots.)
>
> These requirements apply to the modified work as a whole.  If
> identifiable sections of that work are not derived from the Library,
> and can be reasonably considered independent and separate works in
> themselves, then this License, and its terms, do not apply to those
> sections when you distribute them as separate works.  But when you
> distribute the same sections as part of a whole which is a work based
> on the Library, the distribution of the whole must be on the terms of
> this License, whose permissions for other licensees extend to the
> entire whole, and thus to each and every part regardless of who wrote
> it.
>
> Thus, it is not the intent of this section to claim rights or contest
> your rights to work written entirely by you; rather, the intent is to
> exercise the right to control the distribution of derivative or
> collective works based on the Library.
>
> In addition, mere aggregation of another work not based on the Library
> with the Library (or with a work based on the Library) on a volume of
> a storage or distribution medium does not bring the other work under
> the scope of this License.
>
>   3. You may opt to apply the terms of the ordinary GNU General Public
> License instead of this License to a given copy of the Library.  To do
> this, you must alter all the notices that refer to this License, so
> that they refer to the ordinary GNU General Public License, version 2,
> instead of to this License.  (If a newer version than version 2 of the
> ordinary GNU General Public License has appeared, then you can specify
> that version instead if you wish.)  Do not make any other change in
> these notices.
>
>   Once this change is made in a given copy, it is irreversible for
> that copy, so the ordinary GNU General Public License applies to all
> subsequent copies and derivative works made from that copy.
>
>   This option is useful when you wish to copy part of the code of
> the Library into a program that is not a library.
>
>   4. You may copy and distribute the Library (or a portion or
> derivative of it, under Section 2) in object code or executable form
> under the terms of Sections 1 and 2 above provided that you accompany
> it with the complete corresponding machine-readable source code, which
> must be distributed under the terms of Sections 1 and 2 above on a
> medium customarily used for software interchange.
>
>   If distribution of object code is made by offering access to copy
> from a designated place, then offering equivalent access to copy the
> source code from the same place satisfies the requirement to
> distribute the source code, even though third parties are not
> compelled to copy the source along with the object code.
>
>   5. A program that contains no derivative of any portion of the
> Library, but is designed to work with the Library by being compiled or
> linked with it, is called a "work that uses the Library".  Such a
> work, in isolation, is not a derivative work of the Library, and
> therefore falls outside the scope of this License.
>
>   However, linking a "work that uses the Library" with the Library
> creates an executable that is a derivative of the Library (because it
> contains portions of the Library), rather than a "work that uses the
> library".  The executable is therefore covered by this License.
> Section 6 states terms for distribution of such executables.
>
>   When a "work that uses the Library" uses material from a header file
> that is part of the Library, the object code for the work may be a
> derivative work of the Library even though the source code is not.
> Whether this is true is especially significant if the work can be
> linked without the Library, or if the work is itself a library.  The
> threshold for this to be true is not precisely defined by law.
>
>   If such an object file uses only numerical parameters, data
> structure layouts and accessors, and small macros and small inline
> functions (ten lines or less in length), then the use of the object
> file is unrestricted, regardless of whether it is legally a derivative
> work.  (Executables containing this object code plus portions of the
> Library will still fall under Section 6.)
>
>   Otherwise, if the work is a derivative of the Library, you may
> distribute the object code for the work under the terms of Section 6.
> Any executables containing that work also fall under Section 6,
> whether or not they are linked directly with the Library itself.
>
>   6. As an exception to the Sections above, you may also compile or
> link a "work that uses the Library" with the Library to produce a
> work containing portions of the Library, and distribute that work
> under terms of your choice, provided that the terms permit
> modification of the work for the customer's own use and reverse
> engineering for debugging such modifications.
>
>   You must give prominent notice with each copy of the work that the
> Library is used in it and that the Library and its use are covered by
> this License.  You must supply a copy of this License.  If the work
> during execution displays copyright notices, you must include the
> copyright notice for the Library among them, as well as a reference
> directing the user to the copy of this License.  Also, you must do one
> of these things:
>
>     a) Accompany the work with the complete corresponding
>     machine-readable source code for the Library including whatever
>     changes were used in the work (which must be distributed under
>     Sections 1 and 2 above); and, if the work is an executable linked
>     with the Library, with the complete machine-readable "work that
>     uses the Library", as object code and/or source code, so that the
>     user can modify the Library and then relink to produce a modified
>     executable containing the modified Library.  (It is understood
>     that the user who changes the contents of definitions files in the
>     Library will not necessarily be able to recompile the application
>     to use the modified definitions.)
>
>     b) Accompany the work with a written offer, valid for at
>     least three years, to give the same user the materials
>     specified in Subsection 6a, above, for a charge no more
>     than the cost of performing this distribution.
>
>     c) If distribution of the work is made by offering access to copy
>     from a designated place, offer equivalent access to copy the above
>     specified materials from the same place.
>
>     d) Verify that the user has already received a copy of these
>     materials or that you have already sent this user a copy.
>
>   For an executable, the required form of the "work that uses the
> Library" must include any data and utility programs needed for
> reproducing the executable from it.  However, as a special exception,
> the source code distributed need not include anything that is normally
> distributed (in either source or binary form) with the major
> components (compiler, kernel, and so on) of the operating system on
> which the executable runs, unless that component itself accompanies
> the executable.
>
>   It may happen that this requirement contradicts the license
> restrictions of other proprietary libraries that do not normally
> accompany the operating system.  Such a contradiction means you cannot
> use both them and the Library together in an executable that you
> distribute.
>
>   7. You may place library facilities that are a work based on the
> Library side-by-side in a single library together with other library
> facilities not covered by this License, and distribute such a combined
> library, provided that the separate distribution of the work based on
> the Library and of the other library facilities is otherwise
> permitted, and provided that you do these two things:
>
>     a) Accompany the combined library with a copy of the same work
>     based on the Library, uncombined with any other library
>     facilities.  This must be distributed under the terms of the
>     Sections above.
>
>     b) Give prominent notice with the combined library of the fact
>     that part of it is a work based on the Library, and explaining
>     where to find the accompanying uncombined form of the same work.
>
>   8. You may not copy, modify, sublicense, link with, or distribute
> the Library except as expressly provided under this License.  Any
> attempt otherwise to copy, modify, sublicense, link with, or
> distribute the Library is void, and will automatically terminate your
> rights under this License.  However, parties who have received copies,
> or rights, from you under this License will not have their licenses
> terminated so long as such parties remain in full compliance.
>
>   9. You are not required to accept this License, since you have not
> signed it.  However, nothing else grants you permission to modify or
> distribute the Library or its derivative works.  These actions are
> prohibited by law if you do not accept this License.  Therefore, by
> modifying or distributing the Library (or any work based on the
> Library), you indicate your acceptance of this License to do so, and
> all its terms and conditions for copying, distributing or modifying
> the Library or works based on it.
>
>   10. Each time you redistribute the Library (or any work based on the
> Library), the recipient automatically receives a license from the
> original licensor to copy, distribute, link with or modify the Library
> subject to these terms and conditions.  You may not impose any further
> restrictions on the recipients' exercise of the rights granted herein.
> You are not responsible for enforcing compliance by third parties to
> this License.
>
>   11. If, as a consequence of a court judgment or allegation of patent
> infringement or for any other reason (not limited to patent issues),
> conditions are imposed on you (whether by court order, agreement or
> otherwise) that contradict the conditions of this License, they do not
> excuse you from the conditions of this License.  If you cannot
> distribute so as to satisfy simultaneously your obligations under this
> License and any other pertinent obligations, then as a consequence you
> may not distribute the Library at all.  For example, if a patent
> license would not permit royalty-free redistribution of the Library by
> all those who receive copies directly or indirectly through you, then
> the only way you could satisfy both it and this License would be to
> refrain entirely from distribution of the Library.
>
> If any portion of this section is held invalid or unenforceable under any
> particular circumstance, the balance of the section is intended to apply,
> and the section as a whole is intended to apply in other circumstances.
>
> It is not the purpose of this section to induce you to infringe any
> patents or other property right claims or to contest validity of any
> such claims; this section has the sole purpose of protecting the
> integrity of the free software distribution system which is
> implemented by public license practices.  Many people have made
> generous contributions to the wide range of software distributed
> through that system in reliance on consistent application of that
> system; it is up to the author/donor to decide if he or she is willing
> to distribute software through any other system and a licensee cannot
> impose that choice.
>
> This section is intended to make thoroughly clear what is believed to
> be a consequence of the rest of this License.
>
>   12. If the distribution and/or use of the Library is restricted in
> certain countries either by patents or by copyrighted interfaces, the
> original copyright holder who places the Library under this License may add
> an explicit geographical distribution limitation excluding those countries,
> so that distribution is permitted only in or among countries not thus
> excluded.  In such case, this License incorporates the limitation as if
> written in the body of this License.
>
>   13. The Free Software Foundation may publish revised and/or new
> versions of the Library General Public License from time to time.
> Such new versions will be similar in spirit to the present version,
> but may differ in detail to address new problems or concerns.
>
> Each version is given a distinguishing version number.  If the Library
> specifies a version number of this License which applies to it and
> "any later version", you have the option of following the terms and
> conditions either of that version or of any later version published by
> the Free Software Foundation.  If the Library does not specify a
> license version number, you may choose any version ever published by
> the Free Software Foundation.
>
>   14. If you wish to incorporate parts of the Library into other free
> programs whose distribution conditions are incompatible with these,
> write to the author to ask for permission.  For software which is
> copyrighted by the Free Software Foundation, write to the Free
> Software Foundation; we sometimes make exceptions for this.  Our
> decision will be guided by the two goals of preserving the free status
> of all derivatives of our free software and of promoting the sharing
> and reuse of software generally.
>
>                             NO WARRANTY
>
>   15. BECAUSE THE LIBRARY IS LICENSED FREE OF CHARGE, THERE IS NO
> WARRANTY FOR THE LIBRARY, TO THE EXTENT PERMITTED BY APPLICABLE LAW.
> EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR
> OTHER PARTIES PROVIDE THE LIBRARY "AS IS" WITHOUT WARRANTY OF ANY
> KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE
> IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
> PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
> LIBRARY IS WITH YOU.  SHOULD THE LIBRARY PROVE DEFECTIVE, YOU ASSUME
> THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
>
>   16. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN
> WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY
> AND/OR REDISTRIBUTE THE LIBRARY AS PERMITTED ABOVE, BE LIABLE TO YOU
> FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR
> CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE
> LIBRARY (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
> RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
> FAILURE OF THE LIBRARY TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
> SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
> DAMAGES.
>
>                      END OF TERMS AND CONDITIONS
"""

# http://www.gnu.org/licenses/gpl-2.0.html
gpl2_terms(pkg::String, copyright::String) =
"""
The $pkg.jl package is licensed under the GNU General Public License, version 2:


> $(copyright)
>
>                   GNU GENERAL PUBLIC LICENSE
>                        Version 2, June 1991
>
>  Copyright (C) 1989, 1991 Free Software Foundation, Inc.,
>  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
>  Everyone is permitted to copy and distribute verbatim copies
>  of this license document, but changing it is not allowed.
>
>                             Preamble
>
>   The licenses for most software are designed to take away your
> freedom to share and change it.  By contrast, the GNU General Public
> License is intended to guarantee your freedom to share and change free
> software--to make sure the software is free for all its users.  This
> General Public License applies to most of the Free Software
> Foundation's software and to any other program whose authors commit to
> using it.  (Some other Free Software Foundation software is covered by
> the GNU Lesser General Public License instead.)  You can apply it to
> your programs, too.
>
>   When we speak of free software, we are referring to freedom, not
> price.  Our General Public Licenses are designed to make sure that you
> have the freedom to distribute copies of free software (and charge for
> this service if you wish), that you receive source code or can get it
> if you want it, that you can change the software or use pieces of it
> in new free programs; and that you know you can do these things.
>
>   To protect your rights, we need to make restrictions that forbid
> anyone to deny you these rights or to ask you to surrender the rights.
> These restrictions translate to certain responsibilities for you if you
> distribute copies of the software, or if you modify it.
>
>   For example, if you distribute copies of such a program, whether
> gratis or for a fee, you must give the recipients all the rights that
> you have.  You must make sure that they, too, receive or can get the
> source code.  And you must show them these terms so they know their
> rights.
>
>   We protect your rights with two steps: (1) copyright the software, and
> (2) offer you this license which gives you legal permission to copy,
> distribute and/or modify the software.
>
>   Also, for each author's protection and ours, we want to make certain
> that everyone understands that there is no warranty for this free
> software.  If the software is modified by someone else and passed on, we
> want its recipients to know that what they have is not the original, so
> that any problems introduced by others will not reflect on the original
> authors' reputations.
>
>   Finally, any free program is threatened constantly by software
> patents.  We wish to avoid the danger that redistributors of a free
> program will individually obtain patent licenses, in effect making the
> program proprietary.  To prevent this, we have made it clear that any
> patent must be licensed for everyone's free use or not licensed at all.
>
>   The precise terms and conditions for copying, distribution and
> modification follow.
>
>                     GNU GENERAL PUBLIC LICENSE
>    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
>
>   0. This License applies to any program or other work which contains
> a notice placed by the copyright holder saying it may be distributed
> under the terms of this General Public License.  The "Program", below,
> refers to any such program or work, and a "work based on the Program"
> means either the Program or any derivative work under copyright law:
> that is to say, a work containing the Program or a portion of it,
> either verbatim or with modifications and/or translated into another
> language.  (Hereinafter, translation is included without limitation in
> the term "modification".)  Each licensee is addressed as "you".
>
> Activities other than copying, distribution and modification are not
> covered by this License; they are outside its scope.  The act of
> running the Program is not restricted, and the output from the Program
> is covered only if its contents constitute a work based on the
> Program (independent of having been made by running the Program).
> Whether that is true depends on what the Program does.
>
>   1. You may copy and distribute verbatim copies of the Program's
> source code as you receive it, in any medium, provided that you
> conspicuously and appropriately publish on each copy an appropriate
> copyright notice and disclaimer of warranty; keep intact all the
> notices that refer to this License and to the absence of any warranty;
> and give any other recipients of the Program a copy of this License
> along with the Program.
>
> You may charge a fee for the physical act of transferring a copy, and
> you may at your option offer warranty protection in exchange for a fee.
>
>   2. You may modify your copy or copies of the Program or any portion
> of it, thus forming a work based on the Program, and copy and
> distribute such modifications or work under the terms of Section 1
> above, provided that you also meet all of these conditions:
>
>     a) You must cause the modified files to carry prominent notices
>     stating that you changed the files and the date of any change.
>
>     b) You must cause any work that you distribute or publish, that in
>     whole or in part contains or is derived from the Program or any
>     part thereof, to be licensed as a whole at no charge to all third
>     parties under the terms of this License.
>
>     c) If the modified program normally reads commands interactively
>     when run, you must cause it, when started running for such
>     interactive use in the most ordinary way, to print or display an
>     announcement including an appropriate copyright notice and a
>     notice that there is no warranty (or else, saying that you provide
>     a warranty) and that users may redistribute the program under
>     these conditions, and telling the user how to view a copy of this
>     License.  (Exception: if the Program itself is interactive but
>     does not normally print such an announcement, your work based on
>     the Program is not required to print an announcement.)
>
> These requirements apply to the modified work as a whole.  If
> identifiable sections of that work are not derived from the Program,
> and can be reasonably considered independent and separate works in
> themselves, then this License, and its terms, do not apply to those
> sections when you distribute them as separate works.  But when you
> distribute the same sections as part of a whole which is a work based
> on the Program, the distribution of the whole must be on the terms of
> this License, whose permissions for other licensees extend to the
> entire whole, and thus to each and every part regardless of who wrote it.
>
> Thus, it is not the intent of this section to claim rights or contest
> your rights to work written entirely by you; rather, the intent is to
> exercise the right to control the distribution of derivative or
> collective works based on the Program.
>
> In addition, mere aggregation of another work not based on the Program
> with the Program (or with a work based on the Program) on a volume of
> a storage or distribution medium does not bring the other work under
> the scope of this License.
>
>   3. You may copy and distribute the Program (or a work based on it,
> under Section 2) in object code or executable form under the terms of
> Sections 1 and 2 above provided that you also do one of the following:
>
>     a) Accompany it with the complete corresponding machine-readable
>     source code, which must be distributed under the terms of Sections
>     1 and 2 above on a medium customarily used for software interchange; or,
>
>     b) Accompany it with a written offer, valid for at least three
>     years, to give any third party, for a charge no more than your
>     cost of physically performing source distribution, a complete
>     machine-readable copy of the corresponding source code, to be
>     distributed under the terms of Sections 1 and 2 above on a medium
>     customarily used for software interchange; or,
>
>     c) Accompany it with the information you received as to the offer
>     to distribute corresponding source code.  (This alternative is
>     allowed only for noncommercial distribution and only if you
>     received the program in object code or executable form with such
>     an offer, in accord with Subsection b above.)
>
> The source code for a work means the preferred form of the work for
> making modifications to it.  For an executable work, complete source
> code means all the source code for all modules it contains, plus any
> associated interface definition files, plus the scripts used to
> control compilation and installation of the executable.  However, as a
> special exception, the source code distributed need not include
> anything that is normally distributed (in either source or binary
> form) with the major components (compiler, kernel, and so on) of the
> operating system on which the executable runs, unless that component
> itself accompanies the executable.
>
> If distribution of executable or object code is made by offering
> access to copy from a designated place, then offering equivalent
> access to copy the source code from the same place counts as
> distribution of the source code, even though third parties are not
> compelled to copy the source along with the object code.
>
>   4. You may not copy, modify, sublicense, or distribute the Program
> except as expressly provided under this License.  Any attempt
> otherwise to copy, modify, sublicense or distribute the Program is
> void, and will automatically terminate your rights under this License.
> However, parties who have received copies, or rights, from you under
> this License will not have their licenses terminated so long as such
> parties remain in full compliance.
>
>   5. You are not required to accept this License, since you have not
> signed it.  However, nothing else grants you permission to modify or
> distribute the Program or its derivative works.  These actions are
> prohibited by law if you do not accept this License.  Therefore, by
> modifying or distributing the Program (or any work based on the
> Program), you indicate your acceptance of this License to do so, and
> all its terms and conditions for copying, distributing or modifying
> the Program or works based on it.
>
>   6. Each time you redistribute the Program (or any work based on the
> Program), the recipient automatically receives a license from the
> original licensor to copy, distribute or modify the Program subject to
> these terms and conditions.  You may not impose any further
> restrictions on the recipients' exercise of the rights granted herein.
> You are not responsible for enforcing compliance by third parties to
> this License.
>
>   7. If, as a consequence of a court judgment or allegation of patent
> infringement or for any other reason (not limited to patent issues),
> conditions are imposed on you (whether by court order, agreement or
> otherwise) that contradict the conditions of this License, they do not
> excuse you from the conditions of this License.  If you cannot
> distribute so as to satisfy simultaneously your obligations under this
> License and any other pertinent obligations, then as a consequence you
> may not distribute the Program at all.  For example, if a patent
> license would not permit royalty-free redistribution of the Program by
> all those who receive copies directly or indirectly through you, then
> the only way you could satisfy both it and this License would be to
> refrain entirely from distribution of the Program.
>
> If any portion of this section is held invalid or unenforceable under
> any particular circumstance, the balance of the section is intended to
> apply and the section as a whole is intended to apply in other
> circumstances.
>
> It is not the purpose of this section to induce you to infringe any
> patents or other property right claims or to contest validity of any
> such claims; this section has the sole purpose of protecting the
> integrity of the free software distribution system, which is
> implemented by public license practices.  Many people have made
> generous contributions to the wide range of software distributed
> through that system in reliance on consistent application of that
> system; it is up to the author/donor to decide if he or she is willing
> to distribute software through any other system and a licensee cannot
> impose that choice.
>
> This section is intended to make thoroughly clear what is believed to
> be a consequence of the rest of this License.
>
>   8. If the distribution and/or use of the Program is restricted in
> certain countries either by patents or by copyrighted interfaces, the
> original copyright holder who places the Program under this License
> may add an explicit geographical distribution limitation excluding
> those countries, so that distribution is permitted only in or among
> countries not thus excluded.  In such case, this License incorporates
> the limitation as if written in the body of this License.
>
>   9. The Free Software Foundation may publish revised and/or new versions
> of the General Public License from time to time.  Such new versions will
> be similar in spirit to the present version, but may differ in detail to
> address new problems or concerns.
>
> Each version is given a distinguishing version number.  If the Program
> specifies a version number of this License which applies to it and "any
> later version", you have the option of following the terms and conditions
> either of that version or of any later version published by the Free
> Software Foundation.  If the Program does not specify a version number of
> this License, you may choose any version ever published by the Free Software
> Foundation.
>
>   10. If you wish to incorporate parts of the Program into other free
> programs whose distribution conditions are different, write to the author
> to ask for permission.  For software which is copyrighted by the Free
> Software Foundation, write to the Free Software Foundation; we sometimes
> make exceptions for this.  Our decision will be guided by the two goals
> of preserving the free status of all derivatives of our free software and
> of promoting the sharing and reuse of software generally.
>
>                             NO WARRANTY
>
>   11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
> FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN
> OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
> PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
> OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
> MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS
> TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE
> PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,
> REPAIR OR CORRECTION.
>
>   12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
> WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
> REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,
> INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING
> OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED
> TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY
> YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER
> PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE
> POSSIBILITY OF SUCH DAMAGES.
>
>                      END OF TERMS AND CONDITIONS
"""

# http://creativecommons.org/publicdomain/zero/1.0/
cc0_terms(pkg::String, copyright::String) =
"""
The $pkg.jl package is available under the CC0 1.0 Universal license:

> $(copyright)
>
> CC0 1.0 Universal
>
>     CREATIVE COMMONS CORPORATION IS NOT A LAW FIRM AND DOES NOT PROVIDE
>     LEGAL SERVICES. DISTRIBUTION OF THIS DOCUMENT DOES NOT CREATE AN
>     ATTORNEY-CLIENT RELATIONSHIP. CREATIVE COMMONS PROVIDES THIS
>     INFORMATION ON AN "AS-IS" BASIS. CREATIVE COMMONS MAKES NO WARRANTIES
>     REGARDING THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS
>     PROVIDED HEREUNDER, AND DISCLAIMS LIABILITY FOR DAMAGES RESULTING FROM
>     THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS PROVIDED
>     HEREUNDER.
>
> Statement of Purpose
>
> The laws of most jurisdictions throughout the world automatically confer
> exclusive Copyright and Related Rights (defined below) upon the creator
> and subsequent owner(s) (each and all, an "owner") of an original work of
> authorship and/or a database (each, a "Work").
>
> Certain owners wish to permanently relinquish those rights to a Work for
> the purpose of contributing to a commons of creative, cultural and
> scientific works ("Commons") that the public can reliably and without fear
> of later claims of infringement build upon, modify, incorporate in other
> works, reuse and redistribute as freely as possible in any form whatsoever
> and for any purposes, including without limitation commercial purposes.
> These owners may contribute to the Commons to promote the ideal of a free
> culture and the further production of creative, cultural and scientific
> works, or to gain reputation or greater distribution for their Work in
> part through the use and efforts of others.
>
> For these and/or other purposes and motivations, and without any
> expectation of additional consideration or compensation, the person
> associating CC0 with a Work (the "Affirmer"), to the extent that he or she
> is an owner of Copyright and Related Rights in the Work, voluntarily
> elects to apply CC0 to the Work and publicly distribute the Work under its
> terms, with knowledge of his or her Copyright and Related Rights in the
> Work and the meaning and intended legal effect of CC0 on those rights.
>
> 1. Copyright and Related Rights. A Work made available under CC0 may be
> protected by copyright and related or neighboring rights ("Copyright and
> Related Rights"). Copyright and Related Rights include, but are not
> limited to, the following:
>
>   i. the right to reproduce, adapt, distribute, perform, display,
>      communicate, and translate a Work;
>  ii. moral rights retained by the original author(s) and/or performer(s);
> iii. publicity and privacy rights pertaining to a person's image or
>      likeness depicted in a Work;
>  iv. rights protecting against unfair competition in regards to a Work,
>      subject to the limitations in paragraph 4(a), below;
>   v. rights protecting the extraction, dissemination, use and reuse of data
>      in a Work;
>  vi. database rights (such as those arising under Directive 96/9/EC of the
>      European Parliament and of the Council of 11 March 1996 on the legal
>      protection of databases, and under any national implementation
>      thereof, including any amended or successor version of such
>      directive); and
> vii. other similar, equivalent or corresponding rights throughout the
>      world based on applicable law or treaty, and any national
>      implementations thereof.
>
> 2. Waiver. To the greatest extent permitted by, but not in contravention
> of, applicable law, Affirmer hereby overtly, fully, permanently,
> irrevocably and unconditionally waives, abandons, and surrenders all of
> Affirmer's Copyright and Related Rights and associated claims and causes
> of action, whether now known or unknown (including existing as well as
> future claims and causes of action), in the Work (i) in all territories
> worldwide, (ii) for the maximum duration provided by applicable law or
> treaty (including future time extensions), (iii) in any current or future
> medium and for any number of copies, and (iv) for any purpose whatsoever,
> including without limitation commercial, advertising or promotional
> purposes (the "Waiver"). Affirmer makes the Waiver for the benefit of each
> member of the public at large and to the detriment of Affirmer's heirs and
> successors, fully intending that such Waiver shall not be subject to
> revocation, rescission, cancellation, termination, or any other legal or
> equitable action to disrupt the quiet enjoyment of the Work by the public
> as contemplated by Affirmer's express Statement of Purpose.
>
> 3. Public License Fallback. Should any part of the Waiver for any reason
> be judged legally invalid or ineffective under applicable law, then the
> Waiver shall be preserved to the maximum extent permitted taking into
> account Affirmer's express Statement of Purpose. In addition, to the
> extent the Waiver is so judged Affirmer hereby grants to each affected
> person a royalty-free, non transferable, non sublicensable, non exclusive,
> irrevocable and unconditional license to exercise Affirmer's Copyright and
> Related Rights in the Work (i) in all territories worldwide, (ii) for the
> maximum duration provided by applicable law or treaty (including future
> time extensions), (iii) in any current or future medium and for any number
> of copies, and (iv) for any purpose whatsoever, including without
> limitation commercial, advertising or promotional purposes (the
> "License"). The License shall be deemed effective as of the date CC0 was
> applied by Affirmer to the Work. Should any part of the License for any
> reason be judged legally invalid or ineffective under applicable law, such
> partial invalidity or ineffectiveness shall not invalidate the remainder
> of the License, and in such case Affirmer hereby affirms that he or she
> will not (i) exercise any of his or her remaining Copyright and Related
> Rights in the Work or (ii) assert any associated claims and causes of
> action with respect to the Work, in either case contrary to Affirmer's
> express Statement of Purpose.
>
> 4. Limitations and Disclaimers.
>
>  a. No trademark or patent rights held by Affirmer are waived, abandoned,
>     surrendered, licensed or otherwise affected by this document.
>  b. Affirmer offers the Work as-is and makes no representations or
>     warranties of any kind concerning the Work, express, implied,
>     statutory or otherwise, including without limitation warranties of
>     title, merchantability, fitness for a particular purpose, non
>     infringement, or the absence of latent or other defects, accuracy, or
>     the present or absence of errors, whether or not discoverable, all to
>     the greatest extent permissible under applicable law.
>  c. Affirmer disclaims responsibility for clearing rights of other persons
>     that may apply to the Work or any use thereof, including without
>     limitation any person's Copyright and Related Rights in the Work.
>     Further, Affirmer disclaims responsibility for obtaining any necessary
>     consents, permissions or other rights required for any use of the
>     Work.
>  d. Affirmer understands and acknowledges that Creative Commons is not a
>     party to this document and has no duty or obligation with respect to
>     this CC0 or use of the Work.
"""
end
