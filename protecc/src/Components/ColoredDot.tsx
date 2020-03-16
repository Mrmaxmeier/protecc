import React, { FC } from 'react';

export const ColoredDot: FC<{ size?: string, color: string }> = (props) => {
    return <span style={{
        height: props.size || '1em',
        width: props.size || '1em',
        marginRight: '0.3em',
        backgroundColor: props.color,
        borderRadius: '50%',
        display: 'inline-block'
    }}
    />
}