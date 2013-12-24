package com.backlightcontroller;

import android.widget.SeekBar;
import android.widget.SeekBar.OnSeekBarChangeListener;

/**
 * @author Khalil Fazal
 */
public class BacklightAdjuster implements OnSeekBarChangeListener {

    /**
     * The max.
     */
    private final int max;

    /**
     * Instantiates a new backlight adjuster.
     *
     * @param max the max
     */
    public BacklightAdjuster(final int max) {
        this.max = max;
    }

    /**
     * progress = [0 .. 10000]
     * 
     * @see android.widget.SeekBar.OnSeekBarChangeListener#onProgressChanged(android.widget.SeekBar, int, boolean)
     */
    @Override
    public void onProgressChanged(final SeekBar seekBar, final int level, final boolean fromUser) {
        new SendBrightness(this.getProgress(level)).start();
    }

    /**
     * Gets the progress.
     *
     * @param level the level
     * @return the progress
     */
    private double getProgress(final int level) {
        return Math.pow(this.max, (double) level / this.max - 1);
    }

    /**
     * @see android.widget.SeekBar.OnSeekBarChangeListener#onStartTrackingTouch(android.widget.SeekBar)
     */
    @Override
    public void onStartTrackingTouch(final SeekBar seekBar) {}

    /**
     * @see android.widget.SeekBar.OnSeekBarChangeListener#onStopTrackingTouch(android.widget.SeekBar)
     */
    @Override
    public void onStopTrackingTouch(final SeekBar seekBar) {}

}
