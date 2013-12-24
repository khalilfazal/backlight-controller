package com.backlightcontroller;

import android.app.Activity;
import android.os.Bundle;
import android.widget.SeekBar;

/**
 * @author Khalil Fazal
 */
public class BacklightActivity extends Activity {

    /**
     * @see android.app.Activity#onCreate(android.os.Bundle)
     */
    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        this.setContentView(R.layout.main);
        ((SeekBar) this.findViewById(R.id.seekBar)).setOnSeekBarChangeListener(new BacklightAdjuster());
    }

}