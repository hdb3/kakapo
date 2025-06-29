import matplotlib.pyplot as plt
import numpy as np
import os
import tempfile


def matrix_form(gxx):
    """
    convert nested dicts to matrix form, with vectors for indices

    The outer dict forms the 'i' dimension, inner dict is the 'j' dimension
    For bar plotting this is transpose of the requirment for row of fixed j, i=0..
    premise: the first row got is a template for all others
    """
    group_keys = []
    for group_key in gxx.keys():
        group_keys.append(group_key)
    subgroup_keys = []
    first_row = next(iter(gxx.values()))
    for subgroup_key in first_row.keys():
        subgroup_keys.append(subgroup_key)

    matrix = np.zeros((len(group_keys), len(subgroup_keys)))

    i = 0
    for group_key in group_keys:
        j = 0
        for subgroup_key in subgroup_keys:
            matrix[i][j] = gxx[group_key][subgroup_key]
            j += 1
        i += 1

    return group_keys, subgroup_keys, matrix


"""
premise:
  bars for subgroups set in a single action (ax.bar())
  'X' axis is a subgroup offset [0..1] + [0..n-1]
  where subgroup offset is group index/#group

  the graph constructor iterates over j and produces partial plots with #i elements

  layout horizontal dimension
  elements are spaced/widthed with w=width fixed.  The location of each bar is always a multiple of w=width.
  The width of a column block w_column_block is width * #j+1 to allow for a space between column blocks.
  Call the basic w=width value w_column.

  The location array for elements at j=[0..#j-1]=range(#j) is j*w_column + w_column_block[i, i=0..#i-1] 

"""


def ceildiv(a, b):
    assert a > 0
    assert b > 0
    return (a + b - 1) // b


def bar_plot(group_keys, subgroup_keys, matrix, title, ylabel, legend_text, bar_label=True):

    i_count = len(group_keys)
    j_count = len(subgroup_keys)
    width = 0.25

    y_max = 0

    fig, ax = plt.subplots(layout="constrained")

    for j in range(j_count):
        locations = [width * (j + i * (j_count + 1)) for i in range(i_count)]
        attribute_label = subgroup_keys[j]
        data = matrix[j]
        y_max = max((y_max, max(data)))

        rects = ax.bar(locations, data, width, label=attribute_label)
        if bar_label:
            ax.bar_label(rects, rotation=45, fmt="   %.4g")
        # ax.bar_label(rects, padding=3, fmt="  %.4g")

    ax.set_ylabel(ylabel)
    ax.set_title(title)
    ax.set_xticks([width * (0.5 + i * (j_count + 1)) for i in range(i_count)], group_keys)

    # # horizontal legend layouts
    # ncols = j_count
    # if j_count > 4:
    #     ncols = ceildiv(j_count, 2)

    # # this is the below horizontal legend box solution
    # ax.legend(bbox_to_anchor=(0.5, -0.10), loc="upper center", ncols=ncols, title=legend_text)

    # #  this is the simple horizontal version
    # ax.legend(loc="upper left", ncols=ncols, title=legend_text)

    # this is the vertical legend box solution
    ax.legend(loc="upper left", bbox_to_anchor=(1.02, 1), ncols=1, title=legend_text)

    ax.set_ylim(0, y_max * 1.1)
    fig.show()
    _, path = tempfile.mkstemp(
        suffix=".png",
    )
    os.remove(path)

    plt.savefig(path, dpi=600)
    plt.show()

    return path


def plot(gxx, title, ylabel, legend_text, bar_label=True):
    group_keys, subgroup_keys, matrix = matrix_form(gxx)
    return bar_plot(group_keys, subgroup_keys, matrix.T, title, ylabel, legend_text, bar_label)


def main():

    pm2 = {
        "Bill Depth": {"Adelie": 18.35, "Chinstrap": 18.43, "Gentoo": 14.98},
        "Bill Length": {"Adelie": 38.79, "Chinstrap": 48.83, "Gentoo": 47.50},
        "Flipper Length": {"Adelie": 189.95, "Chinstrap": 195.82, "Gentoo": 217.19},
    }
    ylabel = "Length (mm)"
    title = "Penguin attributes by species"
    legend_text = "metric"

    group_keys, subgroup_keys, matrix = matrix_form(pm2)
    bar_plot(group_keys, subgroup_keys, matrix.T, title, legend_text, ylabel)
    bar_plot(subgroup_keys, group_keys, matrix, title, legend_text, ylabel)


if __name__ == "__main__":
    main()
