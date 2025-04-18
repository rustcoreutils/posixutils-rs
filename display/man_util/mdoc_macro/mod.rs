//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::man_util::parser::Element;
use text_production::StType;
use types::*;

pub mod text_production;
pub mod types;

/// Mdoc language units
#[derive(Debug, Clone, PartialEq)]
pub enum Macro {
    Soi,
    A,
    B,
    C,
    D,
    I,
    J,
    N,
    O,
    P,
    Q,
    R,
    T,
    U,
    V,
    Ad,
    An {
        author_name_type: AnType,
    },
    Ao, // Begin a block enclosed by angle brackets
    Ac, // Close an Ao block
    Ap,
    Aq,
    Ar,
    At,
    Bd {
        block_type: BdType,
        offset: Option<OffsetType>,
        compact: bool,
    },
    Bk,
    Bf(BfType),
    Bl {
        list_type: BlType,
        width: Option<u8>,
        offset: Option<OffsetType>,
        compact: bool,
        columns: Vec<String>,
    },
    Bo,
    Bc, // Close a Bo block
    Bq,
    Bro,
    Brc, // Close a Bro block
    Brq,
    Bsx,
    Bt,
    Bx,
    Cd,
    Cm,
    D1,
    Db, // Obsolete
    Dd,
    Dl,
    Do,
    Dc, // Close a Do block
    Dq,
    Dt {
        title: Option<String>,
        section: String,
        arch: Option<String>,
    },
    Dv,
    Dx,
    Em,
    En,
    Eo {
        opening_delimiter: Option<char>,
        closing_delimiter: Option<char>,
    },
    Ec,
    Er,
    Es {
        // Obsolete
        opening_delimiter: char,
        closing_delimiter: char,
    },
    Ev,
    Ex,
    Fa,
    Fd {
        directive: String,
        arguments: Vec<String>,
    },
    Fl,
    Fn {
        funcname: String,
    },
    Fo {
        funcname: String,
    },
    Fc, // End a function context started by Fo
    Fr, // Obsolete
    Ft,
    Fx,
    Hf,
    Ic,
    In {
        filename: String,
    },
    It {
        head: Vec<Element>,
    },
    Lb {
        lib_name: String,
    },
    Li,
    Lk {
        uri: String,
    },
    Lp,
    Ms,
    Mt,
    Nd,
    Nm {
        name: Option<String>,
    },
    No,
    Ns,
    Nx,
    Oo,
    Oc, // Close multi-line Oo context
    Op,
    Os,
    Ox,
    Pa,
    Pf {
        prefix: String,
    },
    Po,
    Pc, // Close parenthesised context opened by Po
    Pp,
    Pq,
    Ql,
    Qo,
    Qc, // Close quoted context opened by Qo
    Qq,
    Rs,
    Re, // Close an Rs block
    Rv,
    Sh {
        title: String,
    },
    Sm(Option<SmMode>),
    So,
    Sc,
    Sq,
    Ss {
        title: String,
    },
    St(StType),
    Sx,
    Sy,
    Ta,
    Tg {
        term: Option<String>,
    },
    Tn,
    Ud,
    Ux,
    Va,
    Vt,
    Xo,
    Xc, // Close a scope opened by Xo
    Xr {
        name: String,
        section: String,
    },
    _Ed, // End a display context started by Bd
    _Ef, // End a display context started by Bf
    _Ek, // End a keep context started by Bk
    _El, // End a list context started by Bl
    _Ot, // Deprecated
}
