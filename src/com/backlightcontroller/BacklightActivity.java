package com.backlightcontroller;

import java.util.Set;

import android.app.Activity;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.SeekBar;
import android.widget.SeekBar.OnSeekBarChangeListener;

/**
 * The Class BacklightActivity.
 */
public class BacklightActivity extends Activity {

    /**
     * The Constant PREFS_NAME.
     */
    private static final String PREFS_NAME = "Backlight Controller";

    /**
     * The Constant brightness.
     */
    private static final String _brightness = "brightness";

    /**
     * The seek bar.
     */
    protected SeekBar seekBar;

    /**
     * The max.
     */
    protected int max;

    /**
     * The hostname.
     */
    private Hostname hostname;

    /**
     * The hostnames.
     */
    private Set<String> hostnames;

    /**
     * The preferences.
     */
    protected SharedPreferences preferences;

    /**
     * On create.
     *
     * @param savedInstanceState the saved instance state
     * @see android.app.Activity#onCreate(android.os.Bundle)
     */
    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        this.setContentView(R.layout.main);
        this.preferences = this.getSharedPreferences(PREFS_NAME, 0);
        this.seekBar = (SeekBar) this.findViewById(R.id.seekBar);
        this.max = this.seekBar.getMax();
    }

    /**
     * On resume.
     *
     * @see android.app.Activity#onResume() 
     */
    @Override
    protected void onResume() {
        super.onResume();
        this.seekBar.setProgress(this.preferences.getInt(_brightness, this.max));

        this.hostname = new Hostname(this, this.preferences) {
            @Override
            public void callback() {
                final OnSeekBarChangeListener listener = new BacklightAdjuster(this.toString(), BacklightActivity.this.max);
                BacklightActivity.this.seekBar.setOnSeekBarChangeListener(listener);
            }
        };
    }

    /**
     * On pause.
     *
     * @see android.app.Activity#onPause()
     */
    @Override
    protected void onPause() {
        super.onPause();

        final Editor editor = this.preferences.edit();
        editor.putInt(_brightness, this.seekBar.getProgress());
        editor.putStringSet(Hostname._hostnames, this.hostnames);
        editor.putString(Hostname._hostname, this.hostname.toString());
        editor.commit();
    }

    /**
     * On create options menu.
     *
     * @param menu the menu
     * @return true, if successful
     * @see android.app.Activity#onCreateOptionsMenu(android.view.Menu)
     */
    @Override
    public boolean onCreateOptionsMenu(final Menu menu) {
        this.getMenuInflater().inflate(R.menu.menu, menu);
        return super.onCreateOptionsMenu(menu);
    }

    /**
     * On options item selected.
     *
     * @param item the item
     * @return true, if successful
     * @see android.app.Activity#onOptionsItemSelected(android.view.MenuItem)
     */
    @Override
    public boolean onOptionsItemSelected(final MenuItem item) {
        switch (item.getItemId()) {
            case R.id.hostname:
                this.hostname.get("");
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }
}