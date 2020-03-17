import React from 'react';
import { Union, Literal, Static } from 'runtypes';

export const SemanticColor = Union(
    Literal('red'),
    Literal('orange'),
    Literal('yellow'),
    Literal('olive'),
    Literal('green'),
    Literal('teal'),
    Literal('blue'),
    Literal('violet'),
    Literal('purple'),
    Literal('pink'),
    Literal('brown'),
    Literal('grey'),
    Literal('gray'),
    Literal('black'),
)
export type SemanticColor = Static<typeof SemanticColor>

export function semanticColorToColorcode(color: SemanticColor): string {
    return {
        red: '#db2828',
        orange: '#f2711c',
        yellow: '#fbbd08',
        olive: '#b5cc18',
        green: '#21ba45',
        teal: '#00b5ad',
        blue: '#2185d0',
        violet: '#6435c9',
        purple: '#a333c8',
        pink: '#e03997',
        brown: '#a5673f',
        grey: '#767676',
        gray: '#767676',
        black: '#1b1c1d',
    }[color]
}


interface Props {
    color: string
    useSemanticColors?: boolean
}

export const ColoredLabel: React.FC<Props> = ({ color, useSemanticColors, children }) => {
    let actualColor = useSemanticColors && SemanticColor.guard(color) ? semanticColorToColorcode(color) : color

    return <span style={{
        // => https://react.semantic-ui.com/elements/label/#variations-colored
        backgroundColor: actualColor,
        borderColor: actualColor,
        color: '#fff',
        display: 'inline-block',
        lineHeight: 1,
        verticalAlign: 'baseline',
        margin: "0 .14285714em",
        padding: '.5833em .833em',
        fontWeight: 700,
        borderWidth: 0,
        borderStyle: 'solid',
        borderRadius: '.28571429rem',
        fontSize: '.8em'
    }}>
        {children}
    </span>
}

export const Details: React.FC<{}> = ({ children }) => {
    return <div style={{
        opacity: 0.8,
        marginLeft: '1em',
        verticalAlign: 'top',
        display: 'inline-block',
        fontWeight: 700
    }}>{children}</div>
}