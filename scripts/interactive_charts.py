#!/usr/bin/env python3
"""
Interactive CPI Charts Generator

Creates interactive HTML charts using Plotly for Australian CPI data visualization.
Run after cpi_analysis.R to generate interactive versions of the static charts.
"""

import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import os
from datetime import datetime, timedelta
import numpy as np

# Ensure output directory exists
OUTPUT_DIR = "output"
os.makedirs(OUTPUT_DIR, exist_ok=True)

# RBA Target Band
RBA_TARGET_LOW = 0.02
RBA_TARGET_HIGH = 0.03
RBA_TARGET_MID = 0.025


def load_summary_data():
    """Load the annualised summary CSV if it exists."""
    csv_path = os.path.join(OUTPUT_DIR, "cpi_annualised_summary.csv")
    if os.path.exists(csv_path):
        return pd.read_csv(csv_path)
    return None


def generate_sample_time_series():
    """
    Generate sample time series data for demonstration.
    In production, this would be loaded from cpi_data.rds via R.
    """
    # Generate 5 years of monthly data
    end_date = datetime.now().replace(day=1)
    dates = pd.date_range(end=end_date, periods=60, freq='MS')

    # Simulate realistic inflation patterns
    np.random.seed(42)

    # Base trend with some volatility
    base_trend = np.linspace(0.06, 0.025, len(dates))  # Decreasing from 6% to 2.5%
    noise = np.random.normal(0, 0.005, len(dates))

    headline = base_trend + noise
    monthly = headline + np.random.normal(0, 0.008, len(dates))  # More volatile
    trimmed = headline * 0.95 + np.random.normal(0, 0.003, len(dates))  # Smoother
    weighted = headline * 0.97 + np.random.normal(0, 0.003, len(dates))

    df = pd.DataFrame({
        'date': dates,
        'Headline CPI': headline,
        'Monthly CPI Indicator': monthly,
        'Trimmed Mean': trimmed,
        'Weighted Median': weighted
    })

    return df


def create_inflation_trend_chart(df):
    """Create interactive inflation trend chart with multiple measures."""
    fig = go.Figure()

    # Add RBA target band
    fig.add_hrect(
        y0=RBA_TARGET_LOW, y1=RBA_TARGET_HIGH,
        fillcolor="lightgray", opacity=0.3,
        layer="below", line_width=0,
        annotation_text="RBA Target", annotation_position="top left"
    )

    # Add RBA midpoint
    fig.add_hline(
        y=RBA_TARGET_MID, line_dash="dash",
        line_color="black", line_width=1,
        annotation_text="2.5%"
    )

    colors = {
        'Headline CPI': '#1f77b4',
        'Monthly CPI Indicator': '#ff7f0e',
        'Trimmed Mean': '#2ca02c',
        'Weighted Median': '#d62728'
    }

    for col in df.columns[1:]:
        fig.add_trace(go.Scatter(
            x=df['date'],
            y=df[col],
            mode='lines',
            name=col,
            line=dict(color=colors.get(col, 'gray'), width=2),
            hovertemplate=f'{col}<br>Date: %{{x|%b %Y}}<br>Rate: %{{y:.1%}}<extra></extra>'
        ))

    fig.update_layout(
        title=dict(
            text='Australian Inflation Rates - Annualised',
            font=dict(size=20)
        ),
        xaxis_title='Date',
        yaxis_title='Annualised Inflation Rate',
        yaxis=dict(
            tickformat='.1%',
            range=[-0.02, 0.10]
        ),
        hovermode='x unified',
        legend=dict(
            orientation='h',
            yanchor='bottom',
            y=1.02,
            xanchor='right',
            x=1
        ),
        template='plotly_white',
        height=600
    )

    return fig


def create_comparison_bar_chart(summary_df):
    """Create interactive bar chart comparing annualised rates across periods."""
    if summary_df is None:
        return None

    periods = ['1m_annualised', '3m_annualised', '6m_annualised', '12m_annualised']
    period_labels = ['1-Month', '3-Month', '6-Month', '12-Month']

    fig = go.Figure()

    colors = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728']

    for i, measure in enumerate(summary_df['measure']):
        values = [summary_df[p].iloc[i] if p in summary_df.columns else 0 for p in periods]
        fig.add_trace(go.Bar(
            name=measure,
            x=period_labels,
            y=values,
            marker_color=colors[i % len(colors)],
            hovertemplate=f'{measure}<br>Period: %{{x}}<br>Rate: %{{y:.2%}}<extra></extra>'
        ))

    # Add RBA target band reference lines
    fig.add_hline(y=RBA_TARGET_LOW, line_dash="dash", line_color="green",
                  annotation_text="RBA Target Low (2%)")
    fig.add_hline(y=RBA_TARGET_HIGH, line_dash="dash", line_color="red",
                  annotation_text="RBA Target High (3%)")

    fig.update_layout(
        title='Annualised Inflation by Period',
        xaxis_title='Annualisation Period',
        yaxis_title='Inflation Rate',
        yaxis=dict(tickformat='.1%'),
        barmode='group',
        template='plotly_white',
        height=500,
        legend=dict(orientation='h', yanchor='bottom', y=1.02)
    )

    return fig


def create_persona_comparison_chart():
    """Create interactive chart showing persona-based inflation differences."""
    # Sample persona data
    personas = ['Average Household', 'Vegetarian', 'Car Owner', 'Renter', 'Homeowner', 'Family']
    rates = [0.032, 0.028, 0.035, 0.038, 0.029, 0.034]

    colors = ['#636EFA' if r <= RBA_TARGET_HIGH else '#EF553B' for r in rates]

    fig = go.Figure()

    fig.add_trace(go.Bar(
        x=personas,
        y=rates,
        marker_color=colors,
        text=[f'{r:.1%}' for r in rates],
        textposition='outside',
        hovertemplate='%{x}<br>Inflation: %{y:.2%}<extra></extra>'
    ))

    # Add RBA target band
    fig.add_hrect(
        y0=RBA_TARGET_LOW, y1=RBA_TARGET_HIGH,
        fillcolor="lightgreen", opacity=0.2,
        layer="below", line_width=0
    )

    fig.update_layout(
        title='Personalised Inflation Rates by Household Type',
        xaxis_title='Household Persona',
        yaxis_title='Estimated Annual Inflation',
        yaxis=dict(tickformat='.1%', range=[0, 0.05]),
        template='plotly_white',
        height=500
    )

    return fig


def create_heatmap_chart(summary_df):
    """Create interactive heatmap of inflation rates."""
    if summary_df is None:
        return None

    periods = ['1m_annualised', '3m_annualised', '6m_annualised', '12m_annualised']
    period_labels = ['1-Month', '3-Month', '6-Month', '12-Month']

    # Build matrix
    z_data = []
    for _, row in summary_df.iterrows():
        z_data.append([row[p] if p in summary_df.columns else 0 for p in periods])

    fig = go.Figure(data=go.Heatmap(
        z=z_data,
        x=period_labels,
        y=summary_df['measure'].tolist(),
        colorscale=[
            [0, 'blue'],
            [0.5, 'white'],
            [1, 'red']
        ],
        zmid=RBA_TARGET_MID,
        text=[[f'{v:.1%}' for v in row] for row in z_data],
        texttemplate='%{text}',
        textfont=dict(size=12),
        hovertemplate='%{y}<br>Period: %{x}<br>Rate: %{z:.2%}<extra></extra>'
    ))

    fig.update_layout(
        title='Inflation Heatmap - Annualised Rates',
        xaxis_title='Annualisation Period',
        yaxis_title='Measure',
        template='plotly_white',
        height=400
    )

    return fig


def create_interactive_dashboard():
    """Create a combined interactive dashboard with all charts."""
    # Load data
    summary_df = load_summary_data()
    time_series_df = generate_sample_time_series()

    # Create subplots
    fig = make_subplots(
        rows=2, cols=2,
        subplot_titles=(
            'Inflation Trend Over Time',
            'Annualised Rates by Period',
            'Persona Comparison',
            'Rate Heatmap'
        ),
        specs=[
            [{"type": "scatter"}, {"type": "bar"}],
            [{"type": "bar"}, {"type": "heatmap"}]
        ],
        vertical_spacing=0.12,
        horizontal_spacing=0.08
    )

    # Add trend lines (row 1, col 1)
    colors = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728']
    for i, col in enumerate(time_series_df.columns[1:]):
        fig.add_trace(
            go.Scatter(
                x=time_series_df['date'],
                y=time_series_df[col],
                mode='lines',
                name=col,
                line=dict(color=colors[i % len(colors)]),
                showlegend=True
            ),
            row=1, col=1
        )

    # Add RBA band to trend chart
    fig.add_hrect(
        y0=RBA_TARGET_LOW, y1=RBA_TARGET_HIGH,
        fillcolor="lightgray", opacity=0.3,
        layer="below", line_width=0,
        row=1, col=1
    )

    # Persona comparison (row 2, col 1)
    personas = ['Average', 'Vegetarian', 'Car Owner', 'Renter']
    rates = [0.032, 0.028, 0.035, 0.038]
    fig.add_trace(
        go.Bar(x=personas, y=rates, marker_color='steelblue', showlegend=False),
        row=2, col=1
    )

    # Update axes
    fig.update_yaxes(tickformat='.1%', row=1, col=1)
    fig.update_yaxes(tickformat='.1%', row=1, col=2)
    fig.update_yaxes(tickformat='.1%', row=2, col=1)

    fig.update_layout(
        title=dict(
            text='Australian CPI Dashboard',
            font=dict(size=24),
            x=0.5
        ),
        height=900,
        template='plotly_white',
        showlegend=True,
        legend=dict(orientation='h', yanchor='bottom', y=-0.1)
    )

    return fig


def main():
    print("Generating interactive CPI charts...")

    # Load data
    summary_df = load_summary_data()
    time_series_df = generate_sample_time_series()

    # Generate individual charts
    charts = {
        'inflation_trend': create_inflation_trend_chart(time_series_df),
        'comparison_bar': create_comparison_bar_chart(summary_df),
        'persona_comparison': create_persona_comparison_chart(),
        'heatmap': create_heatmap_chart(summary_df),
        'dashboard': create_interactive_dashboard()
    }

    # Save charts as HTML
    for name, fig in charts.items():
        if fig is not None:
            output_path = os.path.join(OUTPUT_DIR, f'interactive_{name}.html')
            fig.write_html(output_path, include_plotlyjs=True, full_html=True)
            print(f"  Saved: {output_path}")

    print("\nInteractive charts generated successfully!")
    print(f"Open the HTML files in {OUTPUT_DIR}/ to view the interactive charts.")


if __name__ == '__main__':
    main()
