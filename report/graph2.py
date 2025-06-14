import os
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
from linestyle import linestyle_tuple
import matplotlib.colors as mcolors
from matplotlib.lines import Line2D
from matplotlib.legend import Legend
import inspect


def plot_groups(gxx, plot_text):
    plt.rcParams.update({"font.size": 18})
    plt.rcParams["savefig.directory"] = os.path.dirname(__file__)
    fig, ax = plt.subplots(figsize=(12, 8), layout="constrained")

    colours = iter(mcolors.TABLEAU_COLORS)
    styles = iter(linestyle_tuple)

    group_linestyle = {}
    group_legend_lines = []
    group_labels = []
    subgroup_legend_lines = []
    subgroup_labels = []
    subgroup_colour = {}
    for group, subgroups in gxx.items():
        style = (next(styles))[1]
        group_linestyle[group] = style
        group_legend_lines.append(Line2D([0], [0], linestyle=style))
        group_labels.append(group)

        for subgroup in subgroups:
            if subgroup not in subgroup_colour:
                colour = next(colours)
                subgroup_legend_lines.append(Line2D([0], [0], color=colour))
                subgroup_labels.append(subgroup)
                subgroup_colour[subgroup] = colour

    ax.xaxis.set_major_locator(ticker.MaxNLocator(integer=True))
    ax.set_title(plot_text["title"])
    ax.set_ylabel(plot_text["y_axis"])
    ax.set_xlabel(plot_text["x_axis"])

    for group, subgroups in gxx.items():
        for subgroup, (xs, ys) in subgroups.items():
            print(f"<<<{group}:{subgroup}:{subgroup_colour[subgroup]}>>>")
            ax.plot(xs, ys, label=subgroup, linestyle=group_linestyle[group], color=subgroup_colour[subgroup])

    ax.legend(
        subgroup_legend_lines,
        subgroup_labels,
        loc="upper right",
        title=plot_text["subgroup_title"],
        fontsize=14,
        framealpha=1,
    )
    if len(gxx) > 1:
        ax.add_artist(
            Legend(
                ax,
                group_legend_lines,
                group_labels,
                title=plot_text["group_title"],
                loc="upper left",
                ncols=2,
                fontsize=14,
                framealpha=1,
            )
        )

    # fig.legend( subgroup_legend_lines, subgroup_labels, loc="upper right",title="subgroups" ,frameon=False,fontsize=14)
    # fig.add_artist(Legend(fig, group_legend_lines, group_labels, loc="upper left",title="groups" ,frameon=False,fontsize=14))
    if not ("yfloat" in plot_text and plot_text["yfloat"]):
        ax.set_ylim(bottom=0)
    ax.set_xlim(left=1)
    if "logscalex" in plot_text:
        ax.set_xscale("log")

    fig.show()  # needed to force change in figure layout to accommodate legends
    plt.show()
