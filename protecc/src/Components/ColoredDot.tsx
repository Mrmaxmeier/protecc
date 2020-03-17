import React, { FC } from 'react';
import { SemanticColor, semanticColorToColorcode } from './ColoredLabel';

interface Props {
    size?: string
    useSemanticColors?: boolean
    color: string
}

export const ColoredDot: FC<Props> = ({ size, useSemanticColors, color }) => {
    let actualColor = useSemanticColors && SemanticColor.guard(color) ? semanticColorToColorcode(color) : color

    return <span style={{
        height: size || '1em',
        width: size || '1em',
        marginRight: '0.3em',
        backgroundColor: actualColor,
        borderRadius: '50%',
        display: 'inline-block'
    }}
    />
}