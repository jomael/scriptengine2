unit uSE2Errors;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, {uSE2Types, uSE2OpCode, }uSE2Consts;

type
  TSE2ErrorType   = (petHint, petWarning, petError);

const
  TSE2TokenName : array[TSE2TokenType] of string = (
                    '', 'unknown', 'identifier',
                    // Aritmetic operators
                    '+', '-', '*', '/', 'div', 'mod',
                    // Comparison
                    '=', '<', '<=', '>=', '>', '<>',
                    'is',
                    // Assignment
                    ':=', ':', '.', ';', ',',
                    // Operators
                    'and', 'or', 'xor', 'not', 'shr', 'shl', 'exit', 'break', 'continue',
                    '@',
                    // Unit Positions
                    'program', 'unit', 'interface', 'implementation', 'initialization', 'finalization',
                    'type', 'const', 'var', 'out', 'set', 'record', 'array',
                    // Expressions
                    'uses', 'begin', 'end', 'if', 'then', 'else', 'for', 'to', 'downto', 'do', 'repeat', 'until', 'while', 'case', 'of',
                    // Expression Helpers
                    '(', ')', '[', ']',
                    // Special expressions
                    'try', 'finally', 'except', 'on', 'deprecated',
                    // Class Definitions
                    'class', 'private', 'protected', 'public', 'property', 'virtual', 'abstract', 'override', 'overload',
                    'inherited', 'reintroduce', 'partial', 'helper',
                    // Method definitions
                    'procedure', 'function', 'constructor', 'destructor', 'forward', 'object',
                    // External Methods
                    'external', 'cdecl', 'pascal', 'register', 'safecall', 'stdcall', 'export',
                    // Direct Value Types
                    'string', 'ordinal', 'float', 'nil',

                    // Special Tokens
                    '', 'cast',

                    // Limit
                    ''
                    );

const
  C_HINT_CodeAfterEndIgnored = 'Code after last end is ignored';

  C_ERR_UnexpectedEndOfFile  = 'Unexpected end of file';
  C_ERR_UnkownFileType       = 'Internal error: unknown file type';
  C_ERR_ExpectedButFound     = '%s expected, but found %s instead';
  C_ERR_Unexpected           = 'unexpected token found: %s"';

  C_ERR_SystemUnitNotFound   = 'FATAL ERROR: System unit not available';
  C_ERR_UnitAlreadyAdded     = 'Unit "%s" already added into uses list';
  C_ERR_CouldNotAddUnit      = 'Could not add the unit "%s"';

implementation

end.
