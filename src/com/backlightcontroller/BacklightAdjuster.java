package com.backlightcontroller;

import android.widget.SeekBar;
import android.widget.SeekBar.OnSeekBarChangeListener;

/**
 * @author Khalil Fazal
 */
public class BacklightAdjuster implements OnSeekBarChangeListener {

    /**
     * progress = [0 .. 10000]
     * 
     * @see android.widget.SeekBar.OnSeekBarChangeListener#onProgressChanged(android.widget.SeekBar, int, boolean)
     */
    @Override
    public void onProgressChanged(final SeekBar seekBar, final int level, final boolean fromUser) {
        new SendBrightness((double) level / seekBar.getMax()).start();
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
