use crate::types::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Marat, Sonic, Sestra, Napizdet, Vsosat, Povtori,
    ShoKavo, ShoKavoPon, Pon, Zaebal, Liznut, Otdai, Podsosat,
    Kal, Vonyuchi, Virgin, Fucked, Etot,

    // Built-in Commands
    Televizor, Mazni, Kvadrat, Zirknut, Nazhal, Tyknul, Shara, Grupovuha, Razmer,

    // Data
    Identifier(String), Literal(Value),

    // Operators
    Assign, Plus, Minus, Star, Slash, Mod, Bang,

    // Logic
    Eq, Neq, Lt, Gt, Leq, Geq, And, Or, Dot,

    // Delimiters
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Semicolon, Comma, Colon, Arrow,
    EOF
}